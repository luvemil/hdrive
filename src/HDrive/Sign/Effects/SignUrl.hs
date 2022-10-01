module HDrive.Sign.Effects.SignUrl (
    module HDrive.Sign.Effects.SignUrl.Internal,
    runSignUrlWithAmazonka,
) where

import AWSUtils.Actions (presignObjectKeyGetUrl, presignObjectKeyGetUrls)
import qualified Data.Text.Encoding as T
import HDrive.Sign.Effects.SignUrl.Internal
import Polysemy

runSignUrlWithAmazonka :: Member (Embed IO) r => Sem (SignUrl ': r) a -> Sem r a
runSignUrlWithAmazonka = interpret $ \case
    SignGetUrl conf bucket objPath secsMaybe ->
        embed $
            presignObjectKeyGetUrl conf bucket objPath secsMaybe >>= \case
                Left s -> pure $ Left s
                Right bs -> pure . Right $ T.decodeUtf8 bs
    SignGetUrls conf bucket objPaths secsMaybe ->
        embed $
            presignObjectKeyGetUrls conf bucket objPaths secsMaybe >>= \case
                Left s -> pure $ Left s
                Right bss -> pure . Right $ map T.decodeUtf8 bss