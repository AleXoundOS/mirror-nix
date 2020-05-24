module System.Nix.EnvDrvInfo
  ( EnvDrvInfo(..), OutputName, OutputPath
  , parseEnvDrvInfo
  ) where

import Data.Function ((&))
import Data.Text (Text)
import qualified Data.Text as T

import System.Nix.Derivation


type AttrPath = Text
type OutputName = Text
type OutputPath = Text

data EnvDrvInfo = EnvDrvInfo
  { _attrPath :: !AttrPath
  , _drvPath  :: !DrvPath
  , _outputs  :: ![(OutputName, OutputPath)]
  }
  deriving (Eq, Show)


parseEnvDrvInfo :: Text -> EnvDrvInfo
parseEnvDrvInfo txt =
  case T.words txt of
    [attrPath, drvPath, outputs] ->
      EnvDrvInfo attrPath drvPath (parseOutputs outputs)
    _ -> error $ "unexpected format: " ++ show (T.unpack txt)

parseOutputs :: Text -> [(OutputName, OutputPath)]
parseOutputs = map parseOutput . T.split (== ';')
  where parseOutput txt = ( txt & T.dropWhileEnd (/= '=') & T.dropEnd 1
                          , txt & T.takeWhileEnd (/= '='))
