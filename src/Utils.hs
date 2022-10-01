module Utils where

embedEither :: MonadFail m => Either String a -> m a
embedEither (Left s) = fail s
embedEither (Right a) = pure a

embedMaybe :: MonadFail m => Maybe a -> m a
embedMaybe Nothing = fail "Expected Just but got Nothing"
embedMaybe (Just x) = pure x

groupLeftEither :: Either a (Either b c) -> Either (Either a b) c
groupLeftEither (Left a) = Left (Left a)
groupLeftEither (Right (Left b)) = Left (Right b)
groupLeftEither (Right (Right c)) = Right c