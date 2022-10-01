module Control.Lens.Utils where

import Control.Lens
import Data.Foldable (toList)

unwrapApplicative :: (Traversable t, Applicative f, Foldable f) => [t (f a)] -> [t a]
unwrapApplicative = concatMap (toList . sequenceA)

-- unwrapApplicative ds =
--     ds
--         & traversed %~ sequenceA
--         & concatMap toList

unwrapPrismCouple :: Prism' b c -> [(a, b)] -> [(a, c)]
unwrapPrismCouple thePrism ds =
    ds
        & traversed . _2 %~ preview thePrism
        & unwrapApplicative

unwrapPrismTraversable :: Traversable t => Prism' b c -> [t b] -> [t c]
unwrapPrismTraversable thePrism ds =
    ds
        & traversed . traversed %~ preview thePrism
        & unwrapApplicative

