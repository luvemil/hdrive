module HDrive.DriveAPI.Server where

import AWSUtils.Config (AWSUtilsConfig)
import HDrive.DriveAPI.API
import HDrive.DriveAPI.Handlers
import HDrive.DriveAPI.Types
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Servant

driveAPIServer ::
    ( DriveHandlerConstraint r
    , Member (Error DriveApiError) r
    , Member (Input AWSUtilsConfig) r
    ) =>
    ServerT DriveServerAPI (Sem r)
driveAPIServer =
    freeGetAllStores
        :<|> freeSignNodeUrl
        :<|> freeNodeGetDir
