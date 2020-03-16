module System.Nix.NixToolsProc
  ( nixShowDerivationRec
  ) where

import Data.Text (Text)
import System.Process.Typed

import System.Nix.Derivation


nixShowDerivationRec :: DrvPath -> IO [Derivation]
nixShowDerivationRec = undefined
