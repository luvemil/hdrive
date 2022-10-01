module HDrive.SignAPI.API where

import Data.Text
import HDrive.SignAPI.Types
import Servant

type SignServerAPI =
    "sign" :> QueryParam' '[Required, Strict] "objpath" Text :> Get '[JSON] SignApiResponse
