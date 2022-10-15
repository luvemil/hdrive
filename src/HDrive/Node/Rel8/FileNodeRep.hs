{-# LANGUAGE DeriveAnyClass #-}

module HDrive.Node.Rel8.FileNodeRep where

import Data.Int (Int64)
import GHC.Generics (Generic)
import HDrive.Node.Rel8.DirNodeRep ()
import HDrive.Node.Rel8.Instances ()
import HDrive.Node.Types.DirNode
import HDrive.Node.Types.FileNode
import Rel8

-- TODO: Unify this type with DirRep
data FileNodeRep f = FileNodeRep
    { title :: Column f String
    , fileType :: Column f FileType
    , size :: Column f (Maybe Int64) -- Not necessary to have know the size
    , uuid :: Column f FileId
    , path :: Column f S3FilePath -- S3 file reference
    , thumb :: Column f (Maybe String)
    , dirId :: Column f DirId
    }
    deriving stock (Generic)
    deriving anyclass (Rel8able)

fileNodeRepSchema :: TableSchema (FileNodeRep Name)
fileNodeRepSchema =
    TableSchema
        { name = "file_nodes"
        , schema = Nothing
        , columns =
            FileNodeRep
                { title = "title"
                , fileType = "file_type"
                , size = "size"
                , uuid = "uuid"
                , path = "path"
                , thumb = "thumb"
                , dirId = "dir_id"
                }
        }
