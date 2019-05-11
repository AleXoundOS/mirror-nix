{-# LANGUAGE OverloadedStrings #-}

module System.Nix.Narinfo where

import Data.Text as T (Text, unpack)
import Data.YAML as Y


data NarInfoYaml = NarInfoYaml
  { _storePath   :: FilePath
  , _url         :: !Text -- ^ nar file url compressed or uncompressed
  , _compression :: Text  -- ^ compression type: bz2, xz, none
  , _fileHash    :: !Text -- ^ hash of nar file compressed or uncompressed
  , _fileSize    :: Int
  , _narHash     :: Text  -- ^ uncompressed nar file hash
  , _narSize     :: Int
  , _references  :: !Text -- ^ other narinfo names this references (depends)
  , _deriver     :: Text
  , _sig         :: Text
  } deriving Show


instance FromYAML NarInfoYaml where
  parseYAML (Mapping _ m) = NarInfoYaml
    <$> fmap T.unpack (m .: "StorePath")
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
