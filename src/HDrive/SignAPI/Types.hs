module HDrive.SignAPI.Types where

import Data.Aeson
import qualified Data.Text as T
import GHC.Generics
import Polysemy.Servant.Error

data SignApiResponse = SignApiResponse
    { url :: T.Text
    , version :: String
    }
    deriving (Generic, Show, Eq)

newtype SignApiError = SignApiError String
instance ToAppError SignApiError where
    toAppError (SignApiError msg) = AppError msg Nothing

instance FromJSON SignApiResponse
instance ToJSON SignApiResponse
