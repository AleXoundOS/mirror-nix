{-# LANGUAGE OverloadedStrings #-}

module System.Nix.Narinfo where

import Data.Text (Text)
import Data.YAML as Y


data NarInfoYaml = NarInfoYaml
  { _storePath   :: Text
  , _url         :: !Text
  , _compression :: Text
  , _fileHash    :: !Text
  , _fileSize    :: Int
  , _narHash     :: Text
  , _narSize     :: Int
  , _references  :: !Text
  , _deriver     :: Text
  , _sig         :: Text
  } deriving Show


instance FromYAML NarInfoYaml where
  parseYAML (Mapping _ m) = NarInfoYaml
    <$> m .: "StorePath"
    <*> m .: "URL"
    <*> m .: "Compression"
    <*> m .: "FileHash"
    <*> m .: "FileSize"
    <*> m .: "NarHash"
    <*> m .: "NarSize"
    <*> m .: "References"
    <*> m .: "Deriver"
    <*> m .: "Sig"
  parseYAML _ = fail "fail"
