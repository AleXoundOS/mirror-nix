{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

module System.Nix.Derivation
  ( DerivationP(..), DrvPath
  , DrvIsFixed
  , allDerivationPaths
  , parseJsonDerivations'
  , eitherDecodeStrict'
  , eitherDecodeFileStrict'
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
import Utils


type DrvPath = Text
type DrvIsFixed = Bool

data DerivationP = DerivationP
  { drvOutputs   :: ![(StoreOut, DrvIsFixed)]
  , drvInputSrcs :: ![StorePath]
  , drvInputDrvs ::  HashMap Text [DrvPath]
  , drvEnvPaths  :: ![StoreName]
  }
  deriving (Eq, Show)

instance FromJSON DerivationP where
  parseJSON = withObject "DerivationP" $ \o -> do
    drvOutputs   <- o .: "outputs" >>= parseOutputs
    drvInputSrcs <- o .: "inputSrcs"
    drvInputDrvs <- o .: "inputDrvs"
    drvEnvPaths  <- o .: "env" >>= parseEnv
    return DerivationP{..}

parseOutputs :: Value -> Parser [(StoreOut, DrvIsFixed)]
parseOutputs = withObject "drvOutputs" $ \o ->
  for (HM.toList o) $ \(outName, outAttrs) -> do
    (storePath, isFixed) <- (parseJSON outAttrs <&>) $ \hm ->
      (hm HM.! T.pack "path", T.pack "hash" `HM.member` hm)
    storeName <- either fail pure $ stripParseStoreName storePath
    return ((outName, storeName), isFixed)

parseEnv :: Value -> Parser [StoreName]
parseEnv = withObject "env" $ \o ->
  concatMap rights <$> -- keep only valid 'StoreName's
  for (HM.elems o) (fmap getStoreNamesFromEnv . parseJSON)
  where
    getStoreNamesFromEnv = map stripParseStoreName . T.words

allDerivationPaths :: DerivationP -> [(StoreName, DrvIsFixed)]
allDerivationPaths drv =
  [(storeName, drvIsFixed) | ((_, storeName), drvIsFixed) <- drvOutputs drv]
  ++ zip (inpSrcsPaths ++ inpDrvsPaths) (repeat False)
  where
    inpSrcsPaths = map (forceEitherStr . stripParseStoreName) (drvInputSrcs drv)
    inpDrvsPaths = drvEnvPaths drv

parseJsonDerivations' :: ByteString -> HashMap DrvPath DerivationP
parseJsonDerivations' = either error id . eitherDecodeStrict'
