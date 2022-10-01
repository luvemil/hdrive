module HDrive.DriveAPI.Actions where

import AWSUtils.Config
import Control.Lens (_Just)
import Control.Lens.Operators
import Data.Text (Text)
import qualified Data.Text as T
import HDrive.Node
import HDrive.Sign.Effects.SignUrl
import Polysemy
import Polysemy.Input
import System.FilePath

signFileContent :: (Member SignUrl r, Member (Input AWSUtilsConfig) r) => Store () -> FileNode -> Sem r (Either String Text)
signFileContent store f = do
    conf <- input
    let bucket = T.pack $ store ^. #bucket
        path = T.pack $ (store ^. #path) </> "data" </> (f ^. #uuid . #_FileId)
    signGetUrl conf bucket path (Just 900)

signThumb :: (Member SignUrl r, Member (Input AWSUtilsConfig) r) => Store () -> String -> Sem r (Either String Text)
signThumb store thumb = do
    conf <- input
    let bucket = T.pack $ store ^. #bucket
        path = T.pack $ (store ^. #path) </> "thumbnails" </> thumb
    signGetUrl conf bucket path (Just 900)

signThumbs :: (Member SignUrl r, Member (Input AWSUtilsConfig) r) => Store () -> [String] -> Sem r (Either String [Text])
signThumbs store thumbs = do
    conf <- input
    let bucket = T.pack $ store ^. #bucket
        paths = map (\thumb -> T.pack $ (store ^. #path) </> "thumbnails" </> thumb) thumbs
    signGetUrls conf bucket paths (Just 900)
