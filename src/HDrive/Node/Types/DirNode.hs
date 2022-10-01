module HDrive.Node.Types.DirNode where

import Control.Lens.Operators
import Data.Aeson
import Data.Generics.Labels ()
import Data.Generics.Product ()
import Data.Generics.Sum ()
import Data.UUID (toString)
import Data.UUID.V4 (nextRandom)
import GHC.Generics
import Servant (FromHttpApiData)
import System.FilePath.Lens

data DirNode = DirNode
    { title :: String
    , fullPath :: String
    , thumb :: Maybe String
    , uuid :: DirId
    }
    deriving (Show, Eq, Generic)

instance FromJSON DirNode
instance ToJSON DirNode

newDir :: FilePath -> IO DirNode
newDir fp = do
    let title = fp ^. basename
        fullPath = fp
        thumb = Nothing
    uuid <- DirId . toString <$> nextRandom
    pure DirNode{..}

newtype DirId = DirId String
    deriving (Show, Eq, Ord, Generic)
    deriving newtype (FromJSON, ToJSON, FromHttpApiData)

data ResponseDir a = ResponseDir
    { thisDir :: DirNode
    , parentDir :: Maybe DirId
    , contents :: a
    }
    deriving (Show, Eq, Generic)

instance ToJSON a => ToJSON (ResponseDir a)
