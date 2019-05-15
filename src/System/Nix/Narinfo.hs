{-# LANGUAGE OverloadedStrings #-}

module System.Nix.Narinfo where

import qualified Data.Text as T
import Data.Text (Text)
import Data.YAML as Y


data NarInfo = NarInfo
  { _storePath   :: Text
  , _url         :: !Text -- ^ nar file url compressed or uncompressed
  , _compression :: !NarCompressionType -- ^ compression type: bz2, xz, none
  , _fileHash    :: !Text -- ^ hash of nar file compressed or uncompressed
  , _fileSize    :: Int
  , _narHash     :: Text  -- ^ uncompressed nar file hash
  , _narSize     :: Int
  , _references  :: ![Text] -- ^ other store names this references (depends)
  , _deriver     :: Text
  , _sig         :: Text
  } deriving Show

data StorePath = StorePath
  { _base32hash    :: !Text
  , _pkgNameAndVer :: Text
  } deriving Show

-- | Types of compression supported for NAR archives.
data NarCompressionType = CompBz2 | CompXz | CompNone
  deriving Show

instance FromYAML NarInfo where
  parseYAML (Mapping _ m) = NarInfo
    <$> m .: "StorePath"
    <*> m .: "URL"
    <*> (parseNarCompression =<< m .: "Compression")
    <*> (parseFileHash =<< m .: "FileHash")
    <*> m .: "FileSize"
    <*> m .: "NarHash"
    <*> m .: "NarSize"
    <*> m .: "References"
    <*> m .: "Deriver"
    <*> m .: "Sig"
  parseYAML _ = fail "given bytestring does not begin with yaml map!"

parseNarCompression :: Monad m => Text -> m NarCompressionType
parseNarCompression "xz" = pure CompXz
-- If not using `fail` then what? `empty` does not include a custom message.
parseNarCompression _ = fail "`Compression` type read from Narinfo is not `xz`!"

parseFileHash :: Monad m => Text -> m Text
parseFileHash t = case T.split (== ':') t of
                    ["sha256", base32hash] -> pure base32hash
                    _ -> fail "sha256 `FileHash` cannot be parsed!"

-- parseStoreNames :: Monad m => Text -> m [String]
-- parseStoreNames t = if all True
--                     then pure tWords
--                     else fail $ "invalid store name in " ++ show tWords ++ "!"
--   where
--     tWords = T.words t

mkStorePath :: Text -> Maybe StorePath
mkStorePath t' =
  let (_nixBasePath, p1) = T.splitAt 11 t'
      (base32hash, p2) = T.splitAt 32 p1
      (_hyphen, pkgNameAndVer) = T.splitAt 1 p2
  in StorePath <$> mkBase32 base32hash <*> mkPkgNameAndVer pkgNameAndVer
  where
    mkBase32 :: T.Text -> Maybe T.Text
    mkBase32 t = if all ($ t) [(== 32) . T.length , isBase32]
                 then Just t
                 else Nothing
    mkPkgNameAndVer :: T.Text -> Maybe T.Text
    mkPkgNameAndVer t = if all ($ t) [not . T.null, isAlphaNum]
                        then Just t
                        else Nothing
    isBase32 = T.all (`elem` ("0123456789abcdfghijklmnpqrsvwxyz" :: String))
    isAlphaNum =
      T.all (`elem` ("+-.0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ\
                     \_abcdefghijklmnopqrstuvwxyz" :: String))
