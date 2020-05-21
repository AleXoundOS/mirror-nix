{-# LANGUAGE OverloadedStrings #-}

module System.Nix.FixedOutput
  ( decodeFixedOutputsJson, decodeFixedOutputsJsonFile
  , instantiateFixedOutputs
  , FixedOutputInfo(..), HashType(..)
  ) where

import Data.Aeson
import Data.Aeson.Types (parse)
import Data.ByteString (ByteString)
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Base64 as Base64 (decode)
import qualified Data.Text as T

import System.Nix.NixToolsProc
import qualified System.Nix.Base32 as NixBase32 (encode)


data FixedOutputInfo = FixedOutputInfo
  { _drv  :: !Text
  , _hash :: Text
  , _mode :: Maybe DrvMode
  , _name :: Text
  , _path :: !Text
  , _type :: HashType
  , _urls :: [Text]
  }
  deriving (Eq, Show)

data FixedOutputInfo' = FixedOutputInfo'
  { _drv'  :: !Text
  , _hash' :: Text
  , _mode' :: Maybe DrvMode
  , _name' :: Text
  , _path' :: !Text
  , _type' :: HashType'
  , _urls' :: [Text]
  }
  deriving (Show)

data DrvMode = DrvModeFlat | DrvModeRecursive
  deriving (Eq, Show)

data HashType = HashTypeSha1 | HashTypeSha256 | HashTypeSha512
  deriving (Eq, Show)

data HashType' =
  HashTypeSha1' | HashTypeSha256' | HashTypeSha512' | HashTypeNull'
  deriving (Eq, Show)


instance FromJSON FixedOutputInfo where
  parseJSON = withObject "FixedOutputInfo" $ \v -> FixedOutputInfo'
    <$> v .: "drv"
    <*> v .: "hash"
    <*> v .:? "mode"
    <*> v .: "name"
    <*> v .: "path"
    <*> v .: "type"
    <*> v .: "urls"
    <&> fixHash

instance FromJSON DrvMode where
  parseJSON (String "flat")      = pure DrvModeFlat
  parseJSON (String "recursive") = pure DrvModeRecursive
  parseJSON x = fail $ "unknown drv mode: " ++ show x

instance FromJSON HashType where
  parseJSON (String "sha1")   = pure HashTypeSha1
  parseJSON (String "sha256") = pure HashTypeSha256
  parseJSON (String "sha512") = pure HashTypeSha512
  parseJSON x = fail $ "unknown hash type: " ++ show x

instance FromJSON HashType' where
  parseJSON (String "sha1")   = pure HashTypeSha1'
  parseJSON (String "sha256") = pure HashTypeSha256'
  parseJSON (String "sha512") = pure HashTypeSha512'
  parseJSON Null              = pure HashTypeNull'
  parseJSON x = fail $ "unknown hash type: " ++ show x


-- | Fix `FixedOutputInfo'` by parsing SRI hash if found.
fixHash :: FixedOutputInfo' -> FixedOutputInfo
fixHash fo' =
  FixedOutputInfo{ _drv  = _drv'  fo'
                 , _hash = hash
                 , _mode = _mode' fo'
                 , _name = _name' fo'
                 , _path = _path' fo'
                 , _type = hashType
                 , _urls = _urls' fo'
                 }
  where
    (hashType, hash) =
      case _type' fo' of
        HashTypeSha1'   -> (HashTypeSha1, _hash' fo')
        HashTypeSha256' -> (HashTypeSha256, _hash' fo')
        HashTypeSha512' -> (HashTypeSha512, _hash' fo')
        HashTypeNull' -> parseSriHash (_hash' fo')

parseSriHash :: Text -> (HashType, Text)
parseSriHash sriHash = (fromResult $ parse parseJSON (String hashTypeTxt), hash)
  where
    (hashTypeTxt, hash') = T.break (== '-') sriHash
    hash = NixBase32.encode $ base64Decode' $ encodeUtf8
           $ T.drop 1 hash'
    base64Decode' base64Bs = case Base64.decode base64Bs of
                               Left errStr -> error errStr
                               Right bs' -> bs'
    fromResult (Success val) = val
    fromResult (Error str) = error str

-- | nix-instantiate find-fixed-outputs.nix.
instantiateFixedOutputs :: Nixpkgs -> [NixArg] -> FilePath
                        -> IO [FixedOutputInfo]
instantiateFixedOutputs nixpkgs nixArgsTup script =
  decodeFixedOutputsJson <$> nixInstantiateStrictBs nixpkgs nixArgsTup [script]

decodeFixedOutputsJson :: ByteString -> [FixedOutputInfo]
decodeFixedOutputsJson bs =
  case eitherDecodeStrict bs of
    Right v -> v
    Left s -> error s

decodeFixedOutputsJsonFile :: FilePath -> IO [FixedOutputInfo]
decodeFixedOutputsJsonFile f = do
  eitherDecodeResult <- eitherDecodeFileStrict f
  case eitherDecodeResult of
    Right v -> return v
    Left s -> fail s
