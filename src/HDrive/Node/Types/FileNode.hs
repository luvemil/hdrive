module HDrive.Node.Types.FileNode where

import Data.Aeson (FromJSON (..), ToJSON (..), withText)
import Data.Generics.Labels ()
import Data.Generics.Product ()
import Data.Generics.Sum ()
import Data.Generics.Wrapped ()
import GHC.Generics
import Servant (FromHttpApiData)

data FileType = FileImage | FileVideo | FileOther
    deriving (Show, Eq)

instance FromJSON FileType where
    parseJSON = withText "filetype" $ \case
        "IMAGE" -> pure FileImage
        "VIDEO" -> pure FileVideo
        "OTHER" -> pure FileOther
        s -> fail $ "Cannot parse file type " <> show s
instance ToJSON FileType where
    toJSON FileImage = "IMAGE"
    toJSON FileVideo = "VIDEO"
    toJSON FileOther = "OTHER"

newtype S3FilePath = S3FilePath String
    deriving (Show, Eq, Generic)
    deriving newtype (FromJSON, ToJSON)

newtype FileId = FileId String
    deriving (Show, Eq, Generic)
    deriving newtype (FromJSON, ToJSON, FromHttpApiData)

data FileNode = FileNode
    { title :: String
    , fileType :: FileType
    , size :: Maybe Int -- Not necessary to have know the size
    , uuid :: FileId
    , path :: S3FilePath -- S3 file reference
    , thumb :: Maybe String
    }
    deriving (Show, Eq, Generic)

instance FromJSON FileNode
instance ToJSON FileNode
