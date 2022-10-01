module HDrive.DriveAPI.Handlers where

import AWSUtils.Config (AWSUtilsConfig)
import Control.Lens
import qualified Data.Text as T
import HDrive.DriveAPI.Actions (signThumb, signThumbs)
import HDrive.DriveAPI.Types
import qualified HDrive.Node.Effects.FSStore as FSStore
import HDrive.Node.Types.FS
import HDrive.Node.Types.FileNode (FileId, FileNode)
import HDrive.Node.Types.Store (Store, StoreName)
import HDrive.Sign.Effects.SignUrl (SignUrl, signGetUrl)
import HDrive.SignAPI (SignApiResponse (SignApiResponse))
import Polysemy
import qualified Polysemy.Error as Error
import Polysemy.Input
import Polysemy.Trace
import System.FilePath

type DriveHandlerConstraint r =
    ( Member FSStore.FSStore r
    , Member SignUrl r
    , Member Trace r
    , Member (Error.Error DriveApiError) r
    )

freeNodeGetHandler :: Member (Input FSMap) r => Sem r [FileNode]
freeNodeGetHandler = do
    fsMap <- input
    pure $ fsMap ^.. traversed . traversed . #_FSFile

freeGetAllStores :: DriveHandlerConstraint r => Sem r [Store ()]
freeGetAllStores = do
    trace "freeGetAllStores"
    FSStore.getStores

freeNodeGetDir ::
    ( DriveHandlerConstraint r
    , Member (Input AWSUtilsConfig) r
    ) =>
    StoreName ->
    FilePath ->
    Sem r [FSElem]
freeNodeGetDir sn fp = do
    trace "freeNodeGetDir"
    store <-
        FSStore.getStoreByName sn >>= \case
            -- TODO: use an error type relative to Node
            Nothing -> Error.throw (DriveApiError "Store not found")
            Just s -> pure s
    elems <- FSStore.getContents sn fp
    urls <- signThumbs store $ elems ^.. traversed . _thumb . _Just
    case urls of
        Left _ -> pure elems
        Right thumbUrls -> do
            pure $ elems & partsOf (traversed . _thumb . _Just) .~ map T.unpack thumbUrls

freeSignNodeUrl ::
    ( DriveHandlerConstraint r
    , Member (Input AWSUtilsConfig) r
    ) =>
    StoreName ->
    FileId ->
    Sem r SignApiResponse
freeSignNodeUrl sn fid = do
    trace "freeSignNodeUrl"
    store <-
        FSStore.getStoreByName sn >>= \case
            -- TODO: use an error type relative to Node
            Nothing -> Error.throw (DriveApiError "Store not found")
            Just s -> pure s
    conf <- input
    let bucket = T.pack $ store ^. #bucket
        path = T.pack $ (store ^. #path) </> "data" </> (fid ^. #_FileId)
    res <- -- TODO: change error type and change response type
        over _Right (\t -> SignApiResponse t "1.0")
            . over _Left DriveApiError
            <$> signGetUrl conf bucket path (Just 900)
    Error.fromEither res
