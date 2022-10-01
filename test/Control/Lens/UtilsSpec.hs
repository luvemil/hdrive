module Control.Lens.UtilsSpec where

import Control.Lens
import Control.Lens.Utils
import Control.Monad (replicateM)
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

genElems :: Gen [(String, [Integer])]
genElems = replicateM 20 $ do
    s <- arbitrary
    xs <- vector 5
    pure (s, xs)

prop_sameElements :: Property
prop_sameElements = forAll genElems $ \myData ->
    let x = unwrapApplicative myData ^.. traversed . _2
        x' = myData ^.. traversed . _2 . traversed
     in x `shouldBe` x'

spec :: Spec
{-# NOINLINE spec #-}
spec = describe "Control.Lens.Utils" $ do
    it "unwrapApplicative keeps the same elements" $ property $ prop_sameElements