module HDrive.Node.Rel8.Mappers where

import qualified HDrive.Node.Rel8.DirNodeRep as DirNodeRep
import qualified HDrive.Node.Rel8.FileNodeRep as FileNodeRep
import HDrive.Node.Rel8.StoreRep
import HDrive.Node.Types.DirNode (DirNode (DirNode))
import qualified HDrive.Node.Types.DirNode as DirNode
import HDrive.Node.Types.FileNode (FileNode (FileNode))
import qualified HDrive.Node.Types.FileNode as FileNode
import HDrive.Node.Types.Store (Store (Store))
import qualified HDrive.Node.Types.Store as Store
import Rel8

toStore :: StoreRep Result -> Store ()
toStore s =
    Store
        { Store.storeName = storeName s
        , Store.store = ()
        , Store.bucket = bucket s
        , Store.path = path s
        }

fromStore :: Store a -> StoreRep Result
fromStore s =
    StoreRep
        { storeName = Store.storeName s
        , bucket = Store.bucket s
        , path = Store.path s
        }

toFileNode :: FileNodeRep.FileNodeRep Result -> FileNode
toFileNode f =
    FileNode
        { FileNode.title = FileNodeRep.title f
        , FileNode.fileType = FileNodeRep.fileType f
        , FileNode.size = fromIntegral <$> FileNodeRep.size f
        , FileNode.uuid = FileNodeRep.uuid f
        , FileNode.path = FileNodeRep.path f
        , FileNode.thumb = FileNodeRep.thumb f
        }

fromFileNode :: FileNode -> DirNode.DirId -> FileNodeRep.FileNodeRep Result
fromFileNode f dId =
    FileNodeRep.FileNodeRep
        { FileNodeRep.title = FileNode.title f
        , FileNodeRep.fileType = FileNode.fileType f
        , FileNodeRep.size = fromIntegral <$> FileNode.size f
        , FileNodeRep.uuid = FileNode.uuid f
        , FileNodeRep.path = FileNode.path f
        , FileNodeRep.thumb = FileNode.thumb f
        , FileNodeRep.dirId = dId
        }

toDirNode :: DirNodeRep.DirNodeRep Result -> DirNode
toDirNode f =
    DirNode
        { DirNode.title = DirNodeRep.title f
        , DirNode.fullPath = DirNodeRep.fullPath f
        , DirNode.thumb = DirNodeRep.thumb f
        , DirNode.uuid = DirNodeRep.uuid f
        }

fromDirNode :: DirNode -> Maybe DirNode.DirId -> Store.StoreName -> DirNodeRep.DirNodeRep Result
fromDirNode f dId storeName =
    DirNodeRep.DirNodeRep
        { DirNodeRep.title = DirNode.title f
        , DirNodeRep.fullPath = DirNode.fullPath f
        , DirNodeRep.thumb = DirNode.thumb f
        , DirNodeRep.uuid = DirNode.uuid f
        , DirNodeRep.dirId = dId
        , DirNodeRep.storeName = storeName
        }