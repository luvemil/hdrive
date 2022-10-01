module HDrive.DriveAPI.Types where

import Polysemy.Servant.Error

newtype DriveApiError = DriveApiError String
instance ToAppError DriveApiError where
    toAppError (DriveApiError msg) = AppError msg Nothing
