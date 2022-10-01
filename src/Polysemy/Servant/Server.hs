module Polysemy.Servant.Server where

import Control.Lens
import Control.Monad.Except
import Polysemy
import Polysemy.Error
import Polysemy.Servant.Error (FromAppError (fromAppError), ToAppError (toAppError))
import Polysemy.Trace
import Servant

type ServerEffects = '[Error ServerError, Trace]

interpretSemToIO ::
    Member (Embed IO) r =>
    Sem (Error ServerError : (Trace : r)) a ->
    Sem r (Either ServerError a)
interpretSemToIO sem =
    sem
        & runError @ServerError
        & traceToIO

liftToHandler :: (e -> ServerError) -> IO (Either e a) -> Handler a
liftToHandler f = Handler . ExceptT . fmap (over _Left f)

liftAppErrorToHandler :: ToAppError e => IO (Either e a) -> Handler a
liftAppErrorToHandler = liftToHandler $ fromAppError . toAppError

interpretWithError ::
    (Member (Embed IO) r, ToAppError e) =>
    Sem (Error e : (Trace : r)) a ->
    Sem r (Either ServerError a)
interpretWithError sem =
    sem
        & runError
        & fmap (over _Left (fromAppError . toAppError))
        & traceToIO
