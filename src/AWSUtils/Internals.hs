module AWSUtils.Internals where

import Amazonka
import Amazonka.S3
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Generics.Product
import Data.Maybe (fromMaybe)
import qualified Data.Text.Encoding as T
import Data.Time
import System.IO
import UnliftIO (MonadUnliftIO)

setupEnv ::
    MonadIO m =>
    -- | The credentials to use, if Nothing use Discover
    Maybe Credentials ->
    -- | Region to operate in.
    Region ->
    -- | Endpoint
    ByteString ->
    m Env
setupEnv cred r endPoint = do
    lgr <- newLogger Trace stdout
    newEnv (fromMaybe Discover cred)
        <&> set (field @"_envLogger") lgr
            . set (field @"_envRegion") r
            . override (set (serviceEndpoint . endpointHost) endPoint)

-- TODO: check if the bucket name is already included in the endpoint
concatBucketEndpoint :: BucketName -> ByteString -> ByteString
concatBucketEndpoint bucket endpoint =
    let BucketName bucketT = bucket
        bucketBS = T.encodeUtf8 bucketT
     in bucketBS <> "." <> endpoint

getPresignedURL ::
    MonadUnliftIO m =>
    -- | The credentials to use, if Nothing use Discover
    Maybe Credentials ->
    -- | Region to operate in.
    Region ->
    -- | Endpoint
    ByteString ->
    BucketName ->
    -- | The source object key.
    ObjectKey ->
    -- | Duration of the signature
    Maybe Seconds ->
    m ByteString
getPresignedURL cred r endPoint b k secs = do
    env <- setupEnv cred r $ b `concatBucketEndpoint` endPoint
    ts <- liftIO getCurrentTime
    runResourceT $ presignURL env ts (fromMaybe 60 secs) (newGetObject b k)

getPresignedURLs ::
    MonadUnliftIO m =>
    -- | The credentials to use, if Nothing use Discover
    Maybe Credentials ->
    -- | Region to operate in.
    Region ->
    -- | Endpoint
    ByteString ->
    BucketName ->
    -- | The source object key.
    [ObjectKey] ->
    -- | Duration of the signature
    Maybe Seconds ->
    m [ByteString]
getPresignedURLs cred r endPoint b ks secs = do
    env <- setupEnv cred r $ b `concatBucketEndpoint` endPoint
    ts <- liftIO getCurrentTime
    runResourceT . forM ks $ \k -> presignURL env ts (fromMaybe 60 secs) (newGetObject b k)