module HDrive.Node.FSSpec where

import Control.Lens
import Data.Foldable
import qualified Data.Map as M
import HDrive.Node.FS (addDirDeep, addToLast, ensureDirDoesntEndInSlash)
import HDrive.Node.Types.FS
import System.FilePath
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

-- TODO: get unique keys instead of this
toPaths :: FSMap -> [FilePath]
toPaths fsMap = M.toList fsMap ^.. traversed . _1

spec :: Spec
{-# NOINLINE spec #-}
spec = describe "Dummy test" $ do
    it "fsWithRoot is just the baseFs" $
        fsWithRoot `shouldReturn` baseFS
    it "addDirDeep adds one folder to the structure" $
        fsWithOneDir `shouldReturn` ["/", "/var"]
    it "addDirDeep" $
        computedFS `shouldReturn` ["/", "/path", "/path/to", "/path/to/dir"]
    it "addToLast" $
        addToLast ["/path/to"] "file" `shouldBe` ["/path/to", "/path/to/file"]
    it "computes the aggregated paths" $
        aggregated "/path/to/file" `shouldBe` ["/", "/path", "/path/to", "/path/to/file"]
  where
    baseFS :: FSMap
    baseFS = M.singleton "/" []
    fsWithRoot = addDirDeep baseFS "/"
    fsWithOneDir = toPaths <$> addDirDeep baseFS "/var"
    computedFS =
        toPaths <$> addDirDeep baseFS "/path/to/dir"
    aggregated fp = foldl' addToLast [] (map ensureDirDoesntEndInSlash . splitPath $ "/" </> fp)