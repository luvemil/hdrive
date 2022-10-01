module Polysemy.Servant.Error where

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Servant (ServerError, err401, err500, errBody)

data AppError = AppError {msg :: String, code :: Maybe Int}

class ToAppError e where
    toAppError :: e -> AppError

class FromAppError e where
    fromAppError :: AppError -> e

instance FromAppError ServerError where
    fromAppError (AppError msg (Just 401)) = err401{errBody = TL.encodeUtf8 . TL.pack $ msg}
    fromAppError (AppError msg _) = err500{errBody = TL.encodeUtf8 . TL.pack $ msg}

instance (ToAppError a, ToAppError b) => ToAppError (Either a b) where
    toAppError (Left x) = toAppError x
    toAppError (Right x) = toAppError x
