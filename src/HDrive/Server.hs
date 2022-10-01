module HDrive.Server where

import qualified Data.ByteString.Lazy.Char8 as BS
import HDrive.API
import HDrive.Auth.Types (AuthError (..))
import HDrive.DriveAPI
import HDrive.SignAPI
import Polysemy
import Polysemy.Error (Error, throw)
import Polysemy.Servant.ThrowError ()
import Polysemy.Trace (Trace, trace)
import Servant
import Servant.Auth.Server (AuthResult (..), ThrowAll (throwAll))

hdriveServer :: (SignHandlerConstraint r, DriveHandlerConstraint r) => ServerT HDriveAPI (Sem r)
hdriveServer =
    signAPIServer
        :<|> driveAPIServer

protected ::
    (Member Trace r, Member (Error ServerError) r, SignHandlerConstraint r, DriveHandlerConstraint r) =>
    AuthResult MyAuthResult ->
    ServerT HDriveAPI (Sem r)
protected (Authenticated _) = signAPIServer :<|> driveAPIServer
protected res = throwAll $ err401{errBody = BS.pack (show res)}

-- protected _ = signAPIServer :<|> driveAPIServer

authHDrServer ::
    (Member (Error ServerError) r, SignHandlerConstraint r, DriveHandlerConstraint r) =>
    ServerT (AuthHDrAPI auths) (Sem r)
authHDrServer = protected