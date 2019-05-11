{-# LANGUAGE OverloadedStrings #-}

module System.Nix.Narinfo where

import Data.Text as T (Text, split, unpack)
import Data.YAML as Y


data NarInfo = NarInfo
  { _storePath   :: FilePath
  , _url         :: !Text -- ^ nar file url compressed or uncompressed
  , _compression :: !NarCompressionType -- ^ compression type: bz2, xz, none
  , _fileHash    :: !Text -- ^ hash of nar file compressed or uncompressed
  , _fileSize    :: Int
  , _narHash     :: Text  -- ^ uncompressed nar file hash
  , _narSize     :: Int
  , _references  :: !Text -- ^ other narinfo names this references (depends)
  , _deriver     :: Text
  , _sig         :: Text
  } deriving Show

-- | Types of compression supported for NAR archives.
data NarCompressionType = CompBz2 | CompXz | CompNone
  deriving Show

instance FromYAML NarInfo where
  parseYAML (Mapping _ m) = NarInfo
    <$> fmap T.unpack (m .: "StorePath")
    <*> m .: "URL"
    <*> (mkNarCompression =<< m .: "Compression")
    <*> (mkFileHash =<< m .: "FileHash")
    <*> m .: "FileSize"
    <*> m .: "NarHash"
    <*> m .: "NarSize"
    <*> m .: "References"
    <*> m .: "Deriver"
    <*> m .: "Sig"
  parseYAML _ = fail "given bytestring does not begin with yaml map!"

mkNarCompression :: Monad m => Text -> m NarCompressionType
mkNarCompression "xz" = pure CompXz
-- If not using `fail` then what? `empty` does not include a custom message.
mkNarCompression _ = fail "`Compression` type read from Narinfo is not `xz`!"

mkFileHash :: Monad m => Text -> m Text
mkFileHash t = case T.split (== ':') t of
                 ["sha256", base32hash] -> pure base32hash
                 _ -> fail "sha256 `FileHash` cannot be parsed!"
