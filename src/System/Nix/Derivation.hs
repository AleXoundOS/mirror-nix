module System.Nix.Derivation
  ( Derivation(..), DrvPath, allDerivationPaths
  ) where

import Data.Text (Text)
import Data.Aeson

import System.Nix.StorePath


type DrvPath = Text
data Derivation = Derivation


allDerivationPaths :: Derivation -> [StorePath]
allDerivationPaths drv = undefined
