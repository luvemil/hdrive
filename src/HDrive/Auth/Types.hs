module HDrive.Auth.Types where

import Polysemy.Servant.Error

newtype AuthError = AuthError String
instance ToAppError AuthError where
    toAppError (AuthError msg) = AppError msg (Just 401)