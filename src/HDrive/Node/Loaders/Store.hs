module HDrive.Node.Loaders.Store where

import Data.Aeson
import HDrive.Node.Types.Store
import Utils (embedEither)

loadStoreFromJSON :: FilePath -> IO [Store FilePath]
loadStoreFromJSON fp = embedEither =<< eitherDecodeFileStrict' fp