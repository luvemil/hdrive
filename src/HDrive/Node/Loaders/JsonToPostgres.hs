{-# LANGUAGE TupleSections #-}

module HDrive.Node.Loaders.JsonToPostgres where

import Control.Lens
import Control.Lens.Utils (unwrapApplicative, unwrapPrismTraversable)
import Control.Monad
import qualified Control.Monad.State.Strict as S
import qualified Data.Map.Strict as M
import HDrive.Node.FS (mapToFS)
import HDrive.Node.Loaders.CSV (loadCsvFromFile)
import HDrive.Node.Loaders.Store
import HDrive.Node.Rel8.DirNodeRep (DirNodeRep)
import qualified HDrive.Node.Rel8.DirNodeRep as DirNodeRep
import HDrive.Node.Rel8.FileNodeRep (FileNodeRep)
import HDrive.Node.Rel8.Instances ()
import HDrive.Node.Rel8.Mappers (fromDirNode, fromFileNode, fromStore, toDirNode)
import HDrive.Node.Rel8.StoreRep (StoreRep)
import HDrive.Node.Types.DirNode (DirNode)
import HDrive.Node.Types.FS
import HDrive.Node.Types.FileNode (FileNode)
import HDrive.Node.Types.Store
import Rel8 (Result)

dumpStore :: SMap (DirNodeRep Result) -> String
dumpStore smap =
    let items =
            smap
                & M.toList
                & (traversed . _2 %~ \s -> toDirNode s)
                & M.fromList
     in show items

type SMap t = M.Map (StoreName, FilePath) t

loadDataFromFile :: FilePath -> IO (SMap [FSElem], [Store ()])
loadDataFromFile s3Stores = do
    stores <- loadStoreFromJSON s3Stores
    storeMaps <- stores & traversed . #store %%~ (loadCsvFromFile >=> mapToFS)
    let fsMap =
            storeMaps
                & ( traversed %~ \s ->
                        M.toList (s ^. #store) & traversed . _1 %~ (s ^. #storeName,)
                  )
                & concat
                & M.fromList
    let storeVals = stores & traversed . #store .~ ()
    pure (fsMap, storeVals)

type DirNodeComputation m = S.State (SMap (DirNodeRep Result)) m

addDir :: DirNode -> StoreName -> Maybe FilePath -> DirNodeComputation (Either String (DirNodeRep Result))
addDir d s fpM = do
    store <- S.get
    let parentDirId = do
            f <- fpM
            dirVal <- M.lookup (s, f) store
            pure $ DirNodeRep.uuid dirVal
        newDir = fromDirNode d parentDirId s
        newDirKey = (s, DirNodeRep.fullPath newDir)
    case M.lookup newDirKey store of
        Just _ -> pure $ Left "Directory already found"
        Nothing -> do
            S.put $ M.insert newDirKey newDir store
            pure $ Right newDir

accAddDir :: Either String [DirNodeRep Result] -> ((StoreName, FilePath), DirNode) -> DirNodeComputation (Either String [DirNodeRep Result])
accAddDir (Left s) _ = pure $ Left s
accAddDir (Right xs) ((s, f), d) =
    addDir d s (Just f) >>= \case
        Left strg -> pure $ Left strg
        Right newDir -> pure . Right $ xs <> [newDir]

addFile :: FileNode -> StoreName -> Maybe FilePath -> DirNodeComputation (Either String (FileNodeRep Result))
addFile fn s fpM = do
    store <- S.get
    let parentDirId = do
            f <- fpM
            dirVal <- M.lookup (s, f) store
            pure $ DirNodeRep.uuid dirVal
    case parentDirId of
        Nothing ->
            pure . Left $ -- TODO: use a better message
                "Parent directory not found\nfileNode: "
                    <> show fn
                    <> "\nstoreName: "
                    <> show s
                    <> "\nfilePath: "
                    <> show fpM
        -- <> "\nstore dump:\n"
        -- <> dumpStore store
        Just dId -> pure . Right $ fromFileNode fn dId

accAddFile :: Either String [FileNodeRep Result] -> ((StoreName, FilePath), FileNode) -> DirNodeComputation (Either String [FileNodeRep Result])
accAddFile (Left s) _ = pure $ Left s
accAddFile (Right xs) ((s, f), fn) =
    addFile fn s (Just f) >>= \case
        Left str -> pure $ Left str
        Right fnr -> pure . Right $ xs <> [fnr]

initialAddDir :: Either String [DirNodeRep Result]
initialAddDir = Right []

toRel8Rep :: SMap [FSElem] -> [Store ()] -> Either String ([StoreRep Result], [DirNodeRep Result], [FileNodeRep Result])
toRel8Rep fsMap s3Stores =
    let storeReps =
            s3Stores
                & ( traversed %~ \s ->
                        (s ^. #storeName, fromStore s)
                  )
                & M.fromList
        allFileElems = unwrapApplicative $ M.toList fsMap
        allDirNodes = unwrapPrismTraversable #_FSDir allFileElems
        (allDirNodeReps, dirState) =
            allDirNodes
                & foldM accAddDir (Right [])
                & flip S.runState M.empty
        allFileNodes = unwrapPrismTraversable #_FSFile allFileElems
        allFileNodeReps =
            allFileNodes
                & foldM accAddFile (Right [])
                & flip S.evalState dirState
     in case allDirNodeReps of
            Right aDNR -> case allFileNodeReps of
                Left s -> Left s
                Right fnrs ->
                    Right
                        ( storeReps ^.. traversed
                        , aDNR -- _dirReps
                        , fnrs -- _fileReps
                        )
            Left s -> Left s