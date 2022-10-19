module Lib (
    main,
) where

import AWSUtils.Actions (loadAWSUtilsConfig)
import Control.Monad (forM_)
import HDrive.Node.Loaders.JsonToPostgres (loadDataFromFile, toRel8Rep)
import HDrive.Node.Rel8.Actions as Actions
import HDrive.Runner (createApp, createDbApp)
import qualified Hasql.Connection as Hasql
import qualified Hasql.Pool as HP
import qualified Hasql.Transaction as HT
import Hasql.Transaction.Sessions (IsolationLevel (RepeatableRead), Mode (Write), transaction)
import Lib.Cli (loadMode)
import Lib.Db (getConnection, initializeDb, updateDb)
import Lib.Mode
import Network.Wai.Handler.Warp (run)
import System.Environment (getEnv)
import qualified System.Posix as Posix
import qualified Text.Read as T

main :: IO ()
main = do
    loadMode >>= \case
        RunStatic s -> runServer $ FileCliArgs s
        RunDb sett -> runDbServer sett
        Load fp sett -> runLoad fp sett
        Migrate scriptsDir sett -> runMigrate scriptsDir sett

newtype FileCliArgs = FileCliArgs
    { csvStore :: FilePath
    }
    deriving (Show)

runServer :: FileCliArgs -> IO ()
runServer (FileCliArgs csvStorePath) = do
    port <- do
        portMaybe <- (T.readMaybe =<<) <$> Posix.getEnv "PORT"
        case portMaybe of
            Nothing -> pure 3000
            Just n -> pure n

    jwkPath :: FilePath <- getEnv "JWK_PATH"

    config <- loadAWSUtilsConfig
    app <- createApp config csvStorePath jwkPath
    putStrLn $ "Running hdrive on port " <> show port
    run port app

runDbServer :: Hasql.Settings -> IO ()
runDbServer dbOptions = do
    port <- do
        portMaybe <- (T.readMaybe =<<) <$> Posix.getEnv "PORT"
        case portMaybe of
            Nothing -> pure 3000
            Just n -> pure n

    jwkPath :: FilePath <- getEnv "JWK_PATH"

    pool <- HP.acquire (5, 10000000, dbOptions)

    config <- loadAWSUtilsConfig
    app <- createDbApp config pool jwkPath
    putStrLn $ "Running hdrive on port " <> show port
    run port app

runMigrate :: FilePath -> Hasql.Settings -> IO ()
runMigrate fp sett = do
    conn <- getConnection sett
    initializeDb conn
    updateDb conn fp

-- Tentative implementation, move it to a more appropriate place
runLoad :: FilePath -> Hasql.Settings -> IO ()
runLoad fp sett = do
    (fsMap, stores) <- loadDataFromFile fp
    -- START DEBUG CODE
    -- let allFileElems :: [((StoreName, FilePath), FSElem)] = unwrapApplicative $ M.toList fsMap
    --     allDirNodes :: [((StoreName, FilePath), DirNode)] = unwrapPrismTraversable #_FSDir allFileElems
    -- putStrLn "All dirs:"
    -- forM_ allDirNodes $ \((sn, fn), dn) -> do
    --     let s = sn ^. #_StoreName
    --     putStrLn $ s <> "(" <> fn <> "): " <> show dn
    -- END DEBUG CODE
    let rep = toRel8Rep fsMap stores
    (stores', dirNodes, fileNodes) <- case rep of
        Left s -> do
            putStrLn $ "Cannot map to rel8 representations: " <> s
            error "APP_ERROR"
        Right x -> pure x

    putStrLn "Creating pool..."
    pool <- HP.acquire (5, 10000000, sett)

    let runTransaction :: HT.Transaction a -> IO (Either HP.UsageError a)
        runTransaction t = do
            let tS = transaction RepeatableRead Write t
            HP.use pool tS
        chunk :: Int -> [a] -> [[a]]
        chunk n [] = []
        chunk n xs = take n xs : chunk n (drop n xs)
        storesChunks = chunk 10 stores'
        dirNodesChunks = chunk 10 dirNodes
        fileNodesChunks = chunk 50 fileNodes
    -- handleResponse :: String -> Either HP.UsageError Int64 -> IO ()
    -- handleResponse name (Left ue) = case ue of
    --     HP.ConnectionError m_bs -> _
    --     HP.SessionError (HS.QueryError bs txts ce) -> case ce of
    --         ClientError m_bs -> _
    --         ResultError re -> _
    -- handleResponse name (Right ni) = do
    --     putStrLn $ show ni <> name <> " inserted"

    putStrLn "Inserting stores..."
    forM_ storesChunks $ \sc -> do
        res <- runTransaction . HT.statement () $ Actions.putStores sc
        case res of
            Left ue -> do
                putStrLn $ "Error inserting stores:\n" <> show ue
                error "QUERY_ERROR"
            Right ni -> do
                putStrLn $ show ni <> "stores inserted"
                pure ()
    putStrLn "Stores done."

    putStrLn "Inserting dirs..."
    forM_ dirNodesChunks $ \dc -> do
        res <- runTransaction . HT.statement () $ Actions.putDirNodes dc
        case res of
            Left ue -> do
                putStrLn $ "Error inserting dirs:\n" <> show ue
                error "QUERY_ERROR"
            Right ni -> do
                putStrLn $ show ni <> "dirs inserted"
                pure ()
    putStrLn "Dirs done."

    putStrLn "Inserting files..."
    forM_ fileNodesChunks $ \fc -> do
        res <- runTransaction . HT.statement () $ Actions.putFileNodes fc
        case res of
            Left ue -> do
                putStrLn $ "Error inserting files:\n" <> show ue
                error "QUERY_ERROR"
            Right ni -> do
                putStrLn $ show ni <> "files inserted"
                pure ()
    putStrLn "Files done."

    HP.release pool
    putStrLn "Pool released."