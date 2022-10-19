{-# LANGUAGE DeriveAnyClass #-}

module HDrive.Node.Rel8.DirNodeRep where

import GHC.Generics (Generic)
import HDrive.Node.Rel8.Instances ()
import HDrive.Node.Types.DirNode (DirId)
import HDrive.Node.Types.Store (StoreName)
import Rel8 (Column, Name, Rel8able, TableSchema (..))

data DirNodeRep f = DirNodeRep
    { title :: Column f String
    , fullPath :: Column f String
    , thumb :: Column f (Maybe String)
    , uuid :: Column f DirId
    , dirId :: Column f (Maybe DirId)
    , storeName :: Column f StoreName -- TODO: change with storeId
    }
    deriving stock (Generic)
    deriving anyclass (Rel8able)

dirNodeRepSchema :: TableSchema (DirNodeRep Name)
dirNodeRepSchema =
    TableSchema
        { name = "dir_nodes"
        , schema = Nothing
        , columns =
            DirNodeRep
                { title = "title"
                , fullPath = "full_path"
                , thumb = "thumb"
                , uuid = "uuid"
                , dirId = "dir_id"
                , storeName = "store_name"
                }
        }