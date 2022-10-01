module HDrive.Node.Types.FS where

import Control.Lens
import Data.Aeson
import Data.Generics.Labels ()
import Data.Generics.Product ()
import Data.Generics.Sum ()
import Data.Generics.Wrapped ()
import Data.Map (Map)
import GHC.Generics
import HDrive.Node.Types.DirNode (DirNode)
import HDrive.Node.Types.FileNode (FileNode)

data FSElem
    = FSFile FileNode
    | FSDir DirNode
    deriving (Show, Eq, Generic)

_thumb :: Lens' FSElem (Maybe String)
_thumb = lens getter setter
  where
    getter (FSFile x) = x ^. #thumb
    getter (FSDir x) = x ^. #thumb
    setter (FSFile x) thumb = FSFile $ x & #thumb .~ thumb
    setter (FSDir x) thumb = FSDir $ x & #thumb .~ thumb

-- instance FromJSON FSElem where
--     parseJSON v =
--         (FSFile <$> parseJSON v)
--             <|> (FSDir <$> parseJSON v)

instance ToJSON FSElem where
    toJSON (FSFile x) = toJSON x
    toJSON (FSDir x) = toJSON x

type FSMap = Map FilePath [FSElem]
