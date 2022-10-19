{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module HDrive.Runner where

import AWSUtils.Actions (MonadAWSEnv)
import AWSUtils.Config (AWSUtilsConfig)
import Control.Lens
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans.Except (ExceptT (ExceptT), except, runExceptT)
import Data.Aeson (decodeFileStrict')
import Data.IORef (IORef, newIORef)
import qualified Data.Map as M
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import HDrive.API (AuthHDrAPI)
import HDrive.DriveAPI.Types (DriveApiError)
import HDrive.Node.Effects.FSStore (
  runFSStoreAsHasql,
  runFSStoreAsKVStore,
 )
import HDrive.Node.Loaders.JsonToPostgres (loadDataFromFile)
import HDrive.Node.Types.FS (FSElem)
import HDrive.Node.Types.Store (Store, StoreName)
import HDrive.Server (authHDrServer)
import HDrive.Sign.Effects.SignUrl (runSignUrlWithAmazonka)
import HDrive.SignAPI.API (SignServerAPI)
import HDrive.SignAPI.Handlers (handleSignRequest)
import HDrive.SignAPI.Types (SignApiError)
import qualified Hasql.Pool as HP
import Polysemy
import Polysemy.Error (runError)
import Polysemy.Input (runInputConst)
import Polysemy.KVStore (runKVStoreAsState)
import Polysemy.Servant.Server (liftAppErrorToHandler)
import Polysemy.State (runStateIORef)
import Polysemy.Trace (traceToStdout)
import Servant
import Servant.Auth (JWT)
import Servant.Auth.Server (CookieSettings, JWTSettings, defaultCookieSettings, defaultJWTSettings, generateKey)
import Utils (embedMaybe, groupLeftEither)

signServer :: MonadUnliftIO m => ServerT SignServerAPI (ExceptT String (MonadAWSEnv m))
signServer = handleSignRequest

liftServer :: forall m. MonadUnliftIO m => AWSUtilsConfig -> ServerT SignServerAPI (ExceptT String (MonadAWSEnv m)) -> ServerT SignServerAPI (ExceptT ServerError m)
liftServer config = hoistServer (Proxy @SignServerAPI) (interpretServer config)
 where
  interpretServer :: AWSUtilsConfig -> ExceptT String (MonadAWSEnv m) a -> ExceptT ServerError m a
  interpretServer c myApi =
    myApi
      & runExceptT
      & (`runReaderT` c)
      & liftToHandler
  liftToHandler = ExceptT . fmap (over _Left $ \s -> err500{errBody = TL.encodeUtf8 . TL.pack $ s})

createApp :: AWSUtilsConfig -> FilePath -> FilePath -> IO Application
createApp conf s3Stores jwkPath = do
  (fsMap, storeVals) <- loadDataFromFile s3Stores
  fsMapRef <- newIORef fsMap
  myKey <- generateKey
  jwk <- embedMaybe =<< decodeFileStrict' jwkPath
  let app = liftFreeServer conf storeVals fsMapRef
      jwtCfgBase = defaultJWTSettings myKey
      jwtCfg = jwtCfgBase & #validationKeys .~ jwk
      ctx = defaultCookieSettings :. jwtCfg :. EmptyContext
  pure $ serveWithContext (Proxy @(AuthHDrAPI '[JWT])) ctx app

liftFreeServer :: AWSUtilsConfig -> [Store ()] -> IORef (M.Map (StoreName, FilePath) [FSElem]) -> Server (AuthHDrAPI '[JWT])
liftFreeServer config stores storeMap = hoistServerWithContext (Proxy @(AuthHDrAPI '[JWT])) (Proxy @'[CookieSettings, JWTSettings]) (interpretServer config stores storeMap) authHDrServer
 where
  interpretServer conf strs fn api =
    api
      & traceToStdout
      & runInputConst conf
      & runSignUrlWithAmazonka
      & runFSStoreAsKVStore
      & runInputConst strs
      & runKVStoreAsState
      & runStateIORef fn
      & runError @ServerError
      & runError @DriveApiError
      & runError @SignApiError
      & fmap groupLeftEither
      & runM
      & liftAppErrorToHandler
      & liftServerError
   where
    liftServerError :: Handler (Either ServerError a) -> Handler a
    liftServerError h = h >>= \l -> Handler $ except l

createDbApp :: AWSUtilsConfig -> HP.Pool -> FilePath -> IO Application
createDbApp conf pool jwkPath = do
  myKey <- generateKey
  jwk <- embedMaybe =<< decodeFileStrict' jwkPath
  let app = liftFreeDbServer conf pool
      jwtCfgBase = defaultJWTSettings myKey
      jwtCfg = jwtCfgBase & #validationKeys .~ jwk
      ctx = defaultCookieSettings :. jwtCfg :. EmptyContext
  pure $ serveWithContext (Proxy @(AuthHDrAPI '[JWT])) ctx app

liftFreeDbServer :: AWSUtilsConfig -> HP.Pool -> Server (AuthHDrAPI '[JWT])
liftFreeDbServer config connection = hoistServerWithContext (Proxy @(AuthHDrAPI '[JWT])) (Proxy @'[CookieSettings, JWTSettings]) (interpretServer config connection) authHDrServer
 where
  interpretServer conf pool api =
    api
      & traceToStdout
      & runInputConst conf
      & runSignUrlWithAmazonka
      & runFSStoreAsHasql
      & runInputConst pool
      & runError @ServerError
      & runError @DriveApiError
      & runError @SignApiError
      & fmap groupLeftEither
      & runM
      & liftAppErrorToHandler
      & liftServerError
   where
    liftServerError :: Handler (Either ServerError a) -> Handler a
    liftServerError h = h >>= \l -> Handler $ except l