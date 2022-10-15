module Lib.Mode where

import qualified Hasql.Connection as Hasql

data Mode
    = RunStatic FilePath
    | RunDb Hasql.Settings
    | Load FilePath Hasql.Settings
    | Migrate FilePath Hasql.Settings