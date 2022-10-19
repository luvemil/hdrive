{-# LANGUAGE TemplateHaskell #-}

module HDrive.Node.Effects.FSStore where

import Control.Lens.Operators
import Data.Foldable (find)
import Data.Maybe (fromMaybe)
import HDrive.Node.Rel8.Actions (getAllStoresSession, getElementsInFolder)
import qualified HDrive.Node.Rel8.Actions as Actions
import HDrive.Node.Types.FS
import HDrive.Node.Types.Store (Store, StoreName)
import qualified Hasql.Connection as Hasql
import qualified Hasql.Pool as HP
import qualified Hasql.Session as Hasql
import Polysemy
import Polysemy.Input
import Polysemy.KVStore

data FSStore m a where
    GetContents :: StoreName -> FilePath -> FSStore m [FSElem]
    GetStores :: FSStore m [Store ()]
    GetStoreByName :: StoreName -> FSStore m (Maybe (Store ()))

makeSem ''FSStore

-- getStoreByName :: Member FSStore r => StoreName -> Sem r (Maybe (Store ()))
-- getStoreByName sn = find (\s -> s ^. #storeName == sn) <$> getStores

runFSStoreAsKVStore ::
    ( Member (KVStore (StoreName, FilePath) [FSElem]) r
    , Member (Input [Store ()]) r
    ) =>
    Sem (FSStore ': r) a ->
    Sem r a
runFSStoreAsKVStore = interpret $ \case
    GetContents storeName fp ->
        fromMaybe [] <$> lookupKV (storeName, fp)
    GetStores -> input
    GetStoreByName storeName -> find (\s -> s ^. #storeName == storeName) <$> runFSStoreAsKVStore getStores

runFSStoreAsHasql ::
    ( Member (Input HP.Pool) r
    , Member (Embed IO) r
    ) =>
    Sem (FSStore ': r) a ->
    Sem r a
runFSStoreAsHasql = interpret $ \case
    GetContents storeName fp -> do
        pool <- input
        res <- embed . HP.use pool $ getElementsInFolder storeName fp
        case res of
            Left _ -> pure []
            Right sts -> pure sts
    GetStores -> do
        pool <- input
        res <- embed . HP.use pool $ getAllStoresSession
        -- TODO: handle QueryError
        case res of
            Left _ -> pure []
            Right sts -> pure sts
    GetStoreByName storeName -> do
        pool <- input
        res <- embed . HP.use pool $ Actions.getStoreByName storeName
        case res of
            Left _ -> pure Nothing
            Right sts -> pure sts
