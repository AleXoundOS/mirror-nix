{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

module System.Nix.Derivation
  ( DerivationP(..)
  , DrvPath
  , allDerivationPaths
  , eitherDecodeStrict', parseJsonDerivations'
  ) where


import Control.Monad.Fail
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.ByteString (ByteString)
import Data.Either (rights)
import Data.Functor
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Traversable (for)
import Prelude hiding (fail)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

import System.Nix.StoreNames


type DrvPath = Text

data DerivationP = DerivationP
  { drvOutputs   :: ![StoreOut]
  , drvInputSrcs :: ![StorePath]
  , drvInputDrvs ::  HashMap Text [DrvPath]
  , drvEnvPaths  :: ![StoreName]
  }
  deriving (Eq, Show)

instance FromJSON DerivationP where
  parseJSON = withObject "DerivationP" $ \o -> do
    drvOutputs   <- o .:  "outputs" >>= parseOutputs
    drvInputSrcs <- o .:  "inputSrcs"
    drvInputDrvs <- o .:  "inputDrvs"
    drvEnvPaths  <- o .:  "env" >>= parseEnv
    return DerivationP{..}

parseOutputs :: Value -> Parser [StoreOut]
parseOutputs = withObject "drvOutputs" $ \o ->
  for (HM.toList o) $ \(outName, outAttrs) -> do
    storePath <- parseJSON outAttrs <&> (HM.! T.pack "path")
    storeName <- either fail pure $ stripParseStoreName storePath
    return (outName, storeName)

parseEnv :: Value -> Parser [StoreName]
parseEnv = withObject "env" $ \o ->
  concatMap rights <$> -- keep only valid 'StoreName's
  for (HM.elems o) (fmap getStoreNamesFromEnv . parseJSON)
  where
    getStoreNamesFromEnv = map stripParseStoreName . T.words

allDerivationPaths :: DerivationP -> [StoreName]
allDerivationPaths drv =
  map snd (drvOutputs drv)
  ++ rights (map stripParseStoreName (drvInputSrcs drv))
  ++ drvEnvPaths drv

parseJsonDerivations' :: ByteString -> HashMap DrvPath DerivationP
parseJsonDerivations' = either error id . eitherDecodeStrict'
