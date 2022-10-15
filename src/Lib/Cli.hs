module Lib.Cli where

import GHC.Generics
import qualified Hasql.Connection as Hasql
import qualified Lib.Mode as Mode
import Options.Generic

data CliCommand
    = Run
        { dbOpts :: Maybe Hasql.Settings
        , storeFile :: Maybe FilePath
        }
    | Load
        { dbOpts :: Maybe Hasql.Settings
        , storeFile :: Maybe FilePath
        }
    | Update
        { dbOpts :: Maybe Hasql.Settings
        , storeFile :: Maybe FilePath
        }
    deriving (Generic, Show)

instance ParseRecord CliCommand

-- TODO: run in a more generic monad
loadMode :: IO Mode.Mode
loadMode = do
    command :: CliCommand <- getRecord "Get command"
    case command of
        Run (Just opts) _ -> pure $ Mode.RunDb opts
        Run Nothing (Just fp) -> pure $ Mode.RunStatic fp
        Run Nothing Nothing -> do
            putStrLn "Specify server config or store file"
            error "CLI_ARGS"
        Load (Just opts) (Just fp) -> pure $ Mode.Load fp opts
        Load _ _ -> do
            putStrLn "Specify server config and store file"
            error "CLI_ARGS"
        Update (Just opts) (Just fp) -> pure $ Mode.Migrate fp opts
        Update _ _ -> do
            putStrLn "Specify server config and store file"
            error "CLI_ARGS"