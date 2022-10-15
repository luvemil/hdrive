module Lib (
    main,
) where

import AWSUtils.Actions (loadAWSUtilsConfig)
import HDrive.Node.Loaders.JsonToPostgres (loadDataFromFile, toRel8Rep)
import HDrive.Node.Rel8.Actions as Actions
import HDrive.Runner (createApp, createDbApp)
import qualified Hasql.Connection as Hasql
import qualified Hasql.Session as HS
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

    conn <- getConnection dbOptions

    config <- loadAWSUtilsConfig
    app <- createDbApp config conn jwkPath
    putStrLn $ "Running hdrive on port " <> show port
    run port app

runMigrate :: FilePath -> Hasql.Settings -> IO ()
runMigrate fp sett = do
    conn <- getConnection sett
    initializeDb conn
    updateDb conn fp

-- Tentative implementation, move it to a more domain specific place
runLoad :: FilePath -> Hasql.Settings -> IO ()
runLoad fp sett = do
    (fsMap, stores) <- loadDataFromFile fp
    let rep = toRel8Rep fsMap stores
    (stores', dirNodes, fileNodes) <- case rep of
        Left s -> do
            putStrLn $ "Cannot map to rel8 representations: " <> s
            error "APP_ERROR"
        Right x -> pure x
    conn <- getConnection sett
    let q :: HT.Transaction () = do
            _ <- HT.statement () $ Actions.putStores stores'
            _ <- HT.statement () $ Actions.putDirNodes dirNodes
            _ <- HT.statement () $ Actions.putFileNodes fileNodes
            pure ()
        qS = transaction RepeatableRead Write q
    res <- HS.run qS conn
    case res of
        Left qe -> do
            putStrLn $ "Error running query:\n" <> show qe
        Right _ -> pure ()
