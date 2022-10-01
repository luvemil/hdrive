module HDrive.API where

import Data.Aeson.Types (FromJSON, ToJSON)
import GHC.Generics (Generic)
import HDrive.DriveAPI
import HDrive.SignAPI
import Servant
import Servant.Auth.Server (Auth, FromJWT (decodeJWT), ToJWT)

type HDriveAPI = SignServerAPI :<|> DriveServerAPI

-- data AuthUser = AuthUser
--     { username :: String
--     }
--     deriving (Eq, Show, Generic)

newtype MyAuthResult = MyAuthResult ()
    deriving (Eq, Show, Generic)

instance FromJSON MyAuthResult
instance FromJWT MyAuthResult where
    decodeJWT _ = Right $ MyAuthResult ()

instance ToJSON MyAuthResult
instance ToJWT MyAuthResult

type AuthHDrAPI auths = Auth auths MyAuthResult :> HDriveAPI