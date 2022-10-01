module Lib (
    main,
) where

import AWSUtils.Actions (loadAWSUtilsConfig, presignURL)
import Amazonka (FromText (fromText))
import Amazonka.S3 (BucketName, ObjectKey)
import Control.Monad.Reader (ReaderT (runReaderT))
import qualified Data.Text as T
import HDrive.Runner (createApp)
import Network.Wai.Handler.Warp (run)
import Options.Generic
import System.Environment (getEnv)
import qualified System.Posix as Posix
import qualified Text.Read as T
import Utils

data CommandArgs = CommandArgs T.Text T.Text
    deriving (Generic, Show)

instance ParseRecord CommandArgs

parseCommandArgs :: CommandArgs -> Either String (BucketName, ObjectKey)
parseCommandArgs (CommandArgs bucketT pathT) = do
    b <- fromText bucketT
    p <- fromText pathT
    pure (b, p)

someFunc :: IO ()
someFunc = do
    awsUtilConfig <- loadAWSUtilsConfig
    (bucket, path) <-
        embedEither
            . parseCommandArgs
            =<< getRecord "Arguments"
    psUrl <- runReaderT (presignURL bucket path) awsUtilConfig
    putStrLn $ "done: \n" ++ show psUrl

main :: IO ()
main = runServer

newtype FileCliArgs = FileCliArgs
    { csvStore :: FilePath
    }
    deriving (Generic, Show)

instance ParseRecord FileCliArgs

runServer :: IO ()
runServer = do
    port <- do
        portMaybe <- (T.readMaybe =<<) <$> Posix.getEnv "PORT"
        case portMaybe of
            Nothing -> pure 3000
            Just n -> pure n

    FileCliArgs csvStorePath <- getRecord "Cli Args"
    jwkPath :: FilePath <- getEnv "JWK_PATH"

    config <- loadAWSUtilsConfig
    app <- createApp config csvStorePath jwkPath
    putStrLn $ "Running hdrive on port " <> show port
    run port app