module HDrive.SignAPI.Handlers where

import AWSUtils.Actions (MonadAWSEnv, presignImplicitBucket)
import AWSUtils.Config
import Amazonka (FromText (fromText))
import Amazonka.S3.Types (ObjectKey)
import Control.Lens
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader (ask))
import Control.Monad.Trans.Except (ExceptT (ExceptT), throwE)
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import HDrive.SignAPI.Types (SignApiError (SignApiError), SignApiResponse (SignApiResponse))
import Polysemy
import Polysemy.Error (Error)
import qualified Polysemy.Error as Error
import Polysemy.Input (Input, input)

type SignHandlerConstraint r = (Member (Input AWSUtilsConfig) r, Member (Embed IO) r, Member (Error SignApiError) r)

handleSignRequest :: forall m. MonadUnliftIO m => Text -> ExceptT String (MonadAWSEnv m) SignApiResponse
handleSignRequest objpath = do
    conf <- ask
    case fromText @ObjectKey objpath of
        Left s -> throwE s
        Right objectkey -> do
            res <- ExceptT $ presignImplicitBucket conf objectkey
            pure $ SignApiResponse (T.decodeUtf8 res) "1.0"

freeHandleSignRequest ::
    SignHandlerConstraint r =>
    Text ->
    Sem r SignApiResponse
freeHandleSignRequest objpath = do
    conf <- input
    objectKey <- Error.fromEither $ over _Left SignApiError $ fromText @ObjectKey objpath
    res <- Error.fromEitherM $ over _Left SignApiError <$> presignImplicitBucket conf objectKey
    pure $ SignApiResponse (T.decodeUtf8 res) "1.0"