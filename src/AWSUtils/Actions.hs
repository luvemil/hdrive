module AWSUtils.Actions where

import AWSUtils.Config
import AWSUtils.Internals
import Amazonka (Credentials (FromEnv), FromText (fromText))
import Amazonka.S3 (BucketName, ObjectKey)
import Amazonka.Types (Seconds)
import Control.Monad
import Control.Monad.IO.Unlift (MonadIO, MonadUnliftIO, liftIO)
import Control.Monad.Reader (MonadReader (ask), ReaderT)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import System.Environment (getEnv)
import qualified System.Posix as Posix
import Utils

loadAWSUtilsConfig :: MonadIO m => m AWSUtilsConfig
loadAWSUtilsConfig = do
    region <-
        liftIO $
            embedEither
                . fromText
                . T.pack
                =<< getEnv "AWS_REGION"
    endpoint <-
        liftIO $
            B8.pack <$> getEnv "AWS_ENDPOINT"
    let cred = FromEnv "AWS_ACCESS_KEY_ID" "AWS_ACCESS_KEY_SECRET" Nothing (Just "AWS_REGION")
    bucketMS <- liftIO $ Posix.getEnv "AWS_BUCKET"
    let bucket = bucketMS >>= \b -> embedEither . fromText . T.pack $ b
    pure $ AWSUtilsConfig cred region endpoint bucket

type MonadAWSEnv m = ReaderT AWSUtilsConfig m

presignURL :: MonadUnliftIO m => BucketName -> ObjectKey -> MonadAWSEnv m BS.ByteString
presignURL bucket objectKey = do
    AWSUtilsConfig cred region endpoint _ <- ask
    getPresignedURL (Just cred) region endpoint bucket objectKey Nothing

presignExplicitBucket :: MonadUnliftIO m => AWSUtilsConfig -> BucketName -> ObjectKey -> Maybe Seconds -> m (Either String BS.ByteString)
presignExplicitBucket conf bucket objectKey secsM = do
    let AWSUtilsConfig cred region endpoint _ = conf
    res <- getPresignedURL (Just cred) region endpoint bucket objectKey ((Just . fromMaybe 900) secsM)
    pure $ Right res

presignExplicitBucketMany :: MonadUnliftIO m => AWSUtilsConfig -> BucketName -> [ObjectKey] -> Maybe Seconds -> m [BS.ByteString]
presignExplicitBucketMany conf bucket objectKeys secsM = do
    let AWSUtilsConfig cred region endpoint _ = conf
    getPresignedURLs (Just cred) region endpoint bucket objectKeys ((Just . fromMaybe 900) secsM)

presignImplicitBucket :: MonadUnliftIO m => AWSUtilsConfig -> ObjectKey -> m (Either String BS.ByteString)
presignImplicitBucket conf objectKey = do
    let AWSUtilsConfig cred region endpoint bucketM = conf
    case bucketM of
        Just bucket -> do
            res <- getPresignedURL (Just cred) region endpoint bucket objectKey (Just 900)
            pure $ Right res
        Nothing ->
            pure $ Left "Bucket not found"

presignObjectKeyGetUrl :: MonadUnliftIO m => AWSUtilsConfig -> T.Text -> T.Text -> Maybe Seconds -> m (Either String BS.ByteString)
presignObjectKeyGetUrl conf bucket objpath secsM = do
    let bucketNameE = fromText @BucketName bucket
        objectKeyE = fromText @ObjectKey objpath
    case (bucketNameE, objectKeyE) of
        (Right bucketName, Right objectKey) -> liftIO $ presignExplicitBucket conf bucketName objectKey secsM
        _ -> pure $ Left "Cannot parse bucket or object" -- TODO: Improve

presignObjectKeyGetUrls :: MonadUnliftIO m => AWSUtilsConfig -> T.Text -> [T.Text] -> Maybe Seconds -> m (Either String [BS.ByteString])
presignObjectKeyGetUrls conf bucket objpaths secsM = do
    let bucketNameE = fromText @BucketName bucket
        objectKeyE = forM objpaths $ \objpath -> fromText @ObjectKey objpath
    case (bucketNameE, objectKeyE) of
        (Right bucketName, Right objectKeys) -> liftIO $ Right <$> presignExplicitBucketMany conf bucketName objectKeys secsM
        _ -> pure $ Left "Cannot parse bucket or object" -- TODO: Improve
