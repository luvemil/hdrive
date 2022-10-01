module AWSUtils.Config where

import Amazonka (Credentials, Region)
import Amazonka.S3 (BucketName)
import qualified Data.ByteString.Char8 as B8
import Data.Generics.Labels ()
import Data.Generics.Product ()
import Data.Generics.Sum ()
import GHC.Generics

data AWSUtilsConfig = AWSUtilsConfig
    { aucCred :: Credentials
    , aucRegion :: Region
    , aucEndpoint :: B8.ByteString
    , aucBucket :: Maybe BucketName
    }
    deriving (Eq, Show, Generic)
