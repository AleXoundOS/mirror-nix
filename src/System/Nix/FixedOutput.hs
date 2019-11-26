{-# LANGUAGE OverloadedStrings #-}

module System.Nix.FixedOutput
  ( decodeFixedOutputsJsonFile
  , FixedOutputInfo(..), HashType(..)
  ) where

import Data.Aeson
import Data.Text (Text)
import qualified Data.ByteString.Lazy as B (readFile)


data FixedOutputInfo = FixedOutputInfo
  { _drv  :: Text
  , _hash :: !Text
  , _mode :: Maybe DrvMode
  , _name :: Text
  , _path :: Text
  , _type :: !HashType
  , _urls :: ![Text]
  }
  deriving Show

data DrvMode = DrvModeFlat | DrvModeRecursive
  deriving (Eq, Show)

data HashType = HashTypeSha1 | HashTypeSha256 | HashTypeSha512
  deriving (Eq, Show)


instance FromJSON FixedOutputInfo where
  parseJSON = withObject "FixedOutputInfo" $ \v -> FixedOutputInfo
    <$> v .: "drv"
    <*> v .: "hash"
    <*> v .:? "mode"
    <*> v .: "name"
    <*> v .: "path"
    <*> v .: "type"
    <*> v .: "urls"

instance FromJSON DrvMode where
  parseJSON (String "flat") = pure DrvModeFlat
  parseJSON (String "recursive") = pure DrvModeRecursive
  parseJSON x = fail $ "unknown drv mode: " ++ show x

instance FromJSON HashType where
  parseJSON (String "sha1") = pure HashTypeSha1
  parseJSON (String "sha256") = pure HashTypeSha256
  parseJSON (String "sha512") = pure HashTypeSha512
  parseJSON x = fail $ "unknown hash type: " ++ show x

eitherDecodeFileStrict :: (FromJSON a) => FilePath -> IO (Either String a)
eitherDecodeFileStrict = fmap eitherDecode . B.readFile

decodeFixedOutputsJsonFile :: FilePath -> IO [FixedOutputInfo]
decodeFixedOutputsJsonFile f = do
  eitherDecodeResult <- eitherDecodeFileStrict f
  case eitherDecodeResult of
    Right v -> return v
    Left s -> fail s
