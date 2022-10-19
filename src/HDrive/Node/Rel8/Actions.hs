module HDrive.Node.Rel8.Actions where

import Control.Lens.Combinators
import Control.Lens.Operators
import Data.Int (Int64)
import HDrive.Node.Rel8.DirNodeRep (dirNodeRepSchema)
import qualified HDrive.Node.Rel8.DirNodeRep as DirNodeRep
import HDrive.Node.Rel8.FileNodeRep (fileNodeRepSchema)
import qualified HDrive.Node.Rel8.FileNodeRep as FileNodeRep
import HDrive.Node.Rel8.Instances ()
import HDrive.Node.Rel8.Mappers
import HDrive.Node.Rel8.StoreRep (storeRepSchema)
import qualified HDrive.Node.Rel8.StoreRep as StoreRep
import HDrive.Node.Types.FS (FSElem (FSDir, FSFile))
import HDrive.Node.Types.Store
import Hasql.Connection (Connection)
import Hasql.Session
import Hasql.Statement (Statement)
import Rel8 ((&&.), (==.))
import qualified Rel8

getAllStores :: Connection -> IO (Either QueryError [Store ()])
getAllStores conn = do
    let sess = statement () . Rel8.select $ Rel8.each storeRepSchema
        resSession = sess >>= \t -> pure $ map toStore t
    run resSession conn

getAllStoresSession :: Session [Store ()]
getAllStoresSession = do
    storeRes <- statement () . Rel8.select $ Rel8.each storeRepSchema
    pure $ map toStore storeRes

allStoresByName :: StoreName -> Rel8.Query (StoreRep.StoreRep Rel8.Expr)
allStoresByName sn = do
    store <- Rel8.each storeRepSchema
    Rel8.where_ $ StoreRep.storeName store ==. Rel8.lit sn
    pure store

getStoreByName :: StoreName -> Session (Maybe (Store ()))
getStoreByName sn = do
    res <- statement () . Rel8.select $ allStoresByName sn
    pure $ res ^? traversed . to toStore

getElementsInFolder :: StoreName -> FilePath -> Session [FSElem]
getElementsInFolder sn fp = do
    dirs <- statement () . Rel8.select . Rel8.limit 1 $ do
        store <- allStoresByName sn
        dir <- Rel8.each dirNodeRepSchema
        Rel8.where_ $
            DirNodeRep.storeName dir
                ==. StoreRep.storeName store
                &&. DirNodeRep.fullPath dir
                ==. Rel8.lit fp
        pure dir
    case dirs ^? traversed of
        Nothing -> pure []
        Just dir -> do
            let dId = DirNodeRep.uuid dir
            subDirs <- statement () . Rel8.select $ do
                subDir <- Rel8.each dirNodeRepSchema
                Rel8.where_ $ do
                    DirNodeRep.dirId subDir ==. Rel8.lit (Just dId)
                pure subDir
            files <- statement () . Rel8.select $ do
                file <- Rel8.each fileNodeRepSchema
                Rel8.where_ $
                    FileNodeRep.dirId file ==. Rel8.lit dId
                pure file
            pure $
                map (FSDir . toDirNode) subDirs
                    <> map (FSFile . toFileNode) files

-- subDirs <- Rel8.select $ do
--     subDir <- Rel8.each dirNodeRepSchema
--     Rel8.where_

-- TODO: get all subdirs and files and wrap in FSNode

putElems :: (Rel8.Selects names exprs, Rel8.Serializable exprs a) => Rel8.TableSchema names -> [a] -> Statement () Int64
putElems schema items =
    Rel8.insert $
        Rel8.Insert
            { into = schema
            , rows = Rel8.values (map Rel8.lit items)
            , onConflict = Rel8.DoNothing
            , returning = Rel8.NumberOfRowsAffected
            }

putStores :: [StoreRep.StoreRep Rel8.Result] -> Statement () Int64
putStores = putElems storeRepSchema

putDirNodes :: [DirNodeRep.DirNodeRep Rel8.Result] -> Statement () Int64
putDirNodes = putElems dirNodeRepSchema

putFileNodes :: [FileNodeRep.FileNodeRep Rel8.Result] -> Statement () Int64
putFileNodes = putElems fileNodeRepSchema