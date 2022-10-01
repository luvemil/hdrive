{-# LANGUAGE TemplateHaskell #-}

module HDrive.Sign.Effects.SignUrl.Internal where

import AWSUtils.Config (AWSUtilsConfig)
import Amazonka (Seconds)
import Data.Text
import Polysemy

data SignUrl m a where
    SignGetUrl :: AWSUtilsConfig -> Text -> Text -> Maybe Seconds -> SignUrl m (Either String Text)
    SignGetUrls :: AWSUtilsConfig -> Text -> [Text] -> Maybe Seconds -> SignUrl m (Either String [Text])

makeSem ''SignUrl