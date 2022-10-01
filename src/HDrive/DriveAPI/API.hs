module HDrive.DriveAPI.API where

import HDrive.Node.Types.FS (FSElem)
import HDrive.Node.Types.FileNode (FileId)
import HDrive.Node.Types.Store (Store, StoreName)
import HDrive.SignAPI
import Servant

type DriveServerAPI =
    "drive"
        :> ( "stores"
                :> Get '[JSON] [Store ()]
                :<|> Capture' '[Required, Strict] "storeName" StoreName
                    :> "sign"
                    :> QueryParam' '[Required, Strict] "id" FileId
                    :> Get '[JSON] SignApiResponse
                :<|> Capture' '[Required, Strict] "storeName" StoreName
                    :> QueryParam' '[Required, Strict] "path" FilePath
                    :> Get '[JSON] [FSElem]
           )
