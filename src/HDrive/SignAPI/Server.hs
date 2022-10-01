module HDrive.SignAPI.Server where

import HDrive.SignAPI.API
import HDrive.SignAPI.Handlers
import Polysemy
import Servant

signAPIServer :: SignHandlerConstraint r => ServerT SignServerAPI (Sem r)
signAPIServer = freeHandleSignRequest
