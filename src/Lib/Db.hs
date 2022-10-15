module Lib.Db (updateDb, initializeDb, getConnection) where

import Control.Monad.Except (MonadError, MonadIO)
import Control.Monad.Trans.Except
import Data.Foldable (traverse_)
import qualified Hasql.Connection as HC
import Hasql.Migration (MigrationCommand (MigrationInitialization), MigrationError, loadMigrationsFromDirectory, runMigration)
import qualified Hasql.Session as HS
import Hasql.Transaction.Sessions (IsolationLevel (RepeatableRead), Mode (Write), transaction)

type MigrationExec m a = ExceptT (Either HS.QueryError MigrationError) m a

newtype AppMigration a = AppMigration
    { unAppMigration :: MigrationExec IO a
    }
    deriving (Functor, Applicative, Monad, MonadIO, MonadError (Either HS.QueryError MigrationError))

runAppMigration :: AppMigration a -> IO (Either (Either HS.QueryError MigrationError) a)
runAppMigration action = runExceptT (unAppMigration action)

-- TODO: use something like MonadFail m => WriterT o m a instead of IO
unwrapResult :: Either (Either HS.QueryError MigrationError) a -> IO a
unwrapResult (Left r) = case r of
    Left qe -> do
        putStrLn $ "Error running query:\n" <> show qe
        error "QUERY_ERROR"
    Right me -> do
        putStrLn $ "Error running migration:\n" <> show me
        error "MIGRATION_ERROR"
unwrapResult (Right a) = pure a

initializeDb :: HC.Connection -> IO ()
initializeDb conn = do
    putStrLn "Initializing"
    res <- runAppMigration $ migrateM conn MigrationInitialization
    unwrapResult res >> putStrLn "Done."

updateDb :: HC.Connection -> FilePath -> IO ()
updateDb conn scriptsDir = do
    migrations <- loadMigrationsFromDirectory scriptsDir
    res <- runAppMigration $ traverse_ (migrateM conn) migrations
    unwrapResult res >> putStrLn "Done."

migrateM :: HC.Connection -> MigrationCommand -> AppMigration ()
migrateM conn com = AppMigration . ExceptT $ migrate conn com

migrate :: HC.Connection -> MigrationCommand -> IO (Either (Either HS.QueryError MigrationError) ())
migrate conn com = do
    let comT = runMigration com
        -- TODO: verificare IsolationLevel
        comS = transaction RepeatableRead Write comT
    res <- HS.run comS conn
    case res of
        Left qe -> pure $ Left (Left qe)
        Right (Just me) -> pure $ Left (Right me)
        Right Nothing -> pure $ Right ()

getConnection :: HC.Settings -> IO HC.Connection
getConnection sett = do
    connE <- HC.acquire sett
    case connE of
        Left ce -> do
            print ce
            putStrLn "Cannot connect to database. Aborting."
            error "CANNOT_CONNECT"
        Right c -> pure c