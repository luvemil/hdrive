{-# LANGUAGE DeriveFunctor #-}

module HDrive.Node.Types.Store where

import Data.Generics.Labels ()
import Data.Generics.Product ()
import Data.Generics.Sum ()
import GHC.Generics

import Data.Aeson (FromJSON, ToJSON)
import Servant.API (FromHttpApiData)

newtype StoreName = StoreName String
    deriving (Show, Eq, Ord, FromHttpApiData, Generic)
    deriving newtype (ToJSON, FromJSON)

data Store a = Store
    { storeName :: StoreName
    , store :: a
    , bucket :: String
    , path :: String
    }
    deriving (Show, Eq, Generic, Functor)

instance ToJSON a => ToJSON (Store a)
instance FromJSON a => FromJSON (Store a)