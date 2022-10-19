{-# LANGUAGE DeriveAnyClass #-}

module HDrive.Node.Rel8.StoreRep where

import GHC.Generics (Generic)
import HDrive.Node.Rel8.Instances ()
import HDrive.Node.Types.Store (StoreName)
import Rel8

data StoreRep f = StoreRep
    { storeName :: Column f StoreName
    , bucket :: Column f String
    , path :: Column f String
    }
    deriving stock (Generic)
    deriving anyclass (Rel8able)

storeRepSchema :: TableSchema (StoreRep Name)
storeRepSchema =
    TableSchema
        { name = "stores"
        , schema = Nothing
        , columns =
            StoreRep
                { storeName = "store_name"
                , bucket = "bucket"
                , path = "path"
                }
        }