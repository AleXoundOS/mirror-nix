module System.Nix.EnvDrvInfo
  ( EnvDrvInfo(_drvPath, _output)
  , parseEnvDrvInfo
  ) where

import Data.Text (Text)

import System.Nix.Derivation

type OutputName = Text
type OutputPath = Text
type AttrPath = Text

data EnvDrvInfo = EnvDrvInfo
  { _attrPath :: !AttrPath
  , _drvPath  :: !DrvPath
  , _output   :: ![(OutputName, OutputPath)]
  }

parseEnvDrvInfo :: Monad m => Text -> m EnvDrvInfo
parseEnvDrvInfo = undefined
