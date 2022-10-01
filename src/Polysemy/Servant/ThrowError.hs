{-# LANGUAGE UndecidableInstances #-}

module Polysemy.Servant.ThrowError where

import Polysemy
import Polysemy.Error
import Servant.Auth.Server
import Servant.Server (ServerError)

instance {-# OVERLAPPABLE #-} (Member (Error ServerError) r) => ThrowAll (Sem r a) where
    throwAll = throw