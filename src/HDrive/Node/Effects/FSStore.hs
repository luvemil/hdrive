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
    ( Member (Input Hasql.Connection) r
    , Member (Embed IO) r
    ) =>
    Sem (FSStore ': r) a ->
    Sem r a
runFSStoreAsHasql = interpret $ \case
    GetContents storeName fp -> do
        conn <- input
        res <- embed $ Hasql.run (getElementsInFolder storeName fp) conn
        case res of
            Left _ -> pure []
            Right sts -> pure sts
    GetStores -> do
        conn <- input
        res <- embed $ Hasql.run getAllStoresSession conn
        -- TODO: handle QueryError
        case res of
            Left _ -> pure []
            Right sts -> pure sts
    GetStoreByName storeName -> do
        conn <- input
        res <- embed $ Hasql.run (Actions.getStoreByName storeName) conn
        case res of
            Left _ -> pure Nothing
            Right sts -> pure sts
