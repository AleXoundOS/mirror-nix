{-# LANGUAGE OverloadedStrings #-}

module System.Nix.NarInfo where

import qualified Data.Text as T
import Data.Text (Text)
import Data.YAML as Y
import qualified Data.Char as C
import qualified Data.ByteString as B (readFile)


data NarInfo = NarInfo
  { _storeHash   :: StoreHash
  , _url         :: !Text  -- ^ nar file url compressed or uncompressed
  , _compression :: !NarCompressionType -- ^ compression type: bz2, xz, none
  , _fileHash    :: !NarHash  -- ^ sha256 of nar file compressed or uncompressed
  , _fileSize    :: Int
  , _narHash     :: Text   -- ^ uncompressed nar file hash
  , _narSize     :: Int
  , _references  :: ![StoreHash]  -- ^ store hashes this references (depends)
  , _deriver     :: Text
  , _sig         :: Text
  } deriving (Eq, Show)

type StoreName = Text
type StoreHash = Text
type NarHash = Text

-- | Types of compression supported for NAR archives.
data NarCompressionType = CompBz2 | CompXz | CompNone
  deriving (Eq, Show)

instance FromYAML NarInfo where
  parseYAML (Mapping _ m) = validateNarInfo =<< NarInfo
    <$> (parseStorePath =<< m .: "StorePath")
    <*> m .: "URL"
    <*> (parseNarComp   =<< m .: "Compression")
    <*> (parseFileHash  =<< m .: "FileHash")
    <*> m .: "FileSize"
    <*> m .: "NarHash"
    <*> m .: "NarSize"
    <*> (parseRefs      =<< m .:? "References") -- optional
    <*> m .: "Deriver"
    <*> m .: "Sig"
  parseYAML x =
    fail $ "NarInfo YAML parsing Error! \
           \Given ByteString does not begin with YAML map:\n" ++ show x

validateNarInfo :: Monad m => NarInfo -> m NarInfo
validateNarInfo = return

parseNarComp :: Monad m => Text -> m NarCompressionType
parseNarComp "xz" = pure CompXz
parseNarComp t = failWith "`Compression` type read from Narinfo is not `xz`" t

parseFileHash :: Monad m => Text -> m Text
parseFileHash t = case T.split (== ':') t of
                    ["sha256", base32hash] -> pure base32hash
                    _ -> failWith "sha256 `FileHash` cannot be parsed" t

parseStorePath :: Monad m => Text -> m StoreHash
parseStorePath t =
  maybe (failWith "invalid store path" t) pure $ mkStoreHashFromStorePath t

parseRefs :: Monad m => Maybe Text -> m [StoreHash]
parseRefs Nothing = return []
parseRefs (Just t) = maybe (failWith "invalid reference in" t) pure mRefHashes
  where
    mRefHashes = mapM mkStoreHashFromStoreName (T.words t)

mkStoreHashFromStorePath :: Text -> Maybe StoreHash
mkStoreHashFromStorePath t =
  mkStoreHashFromStoreName =<< T.stripPrefix "/nix/store/" t

mkStoreHashFromStoreName :: StoreName -> Maybe StoreHash
mkStoreHashFromStoreName t = if all ($ base32hash) [not . T.null, isBase32]
                                && all ($ rest) [not . T.null, isAlphaNum]
                             then Just base32hash
                             else Nothing
  where
    (base32hash, rest) = T.splitAt 32 t
    isBase32 = T.all (`elem` ("0123456789abcdfghijklmnpqrsvwxyz" :: String))
    isAlphaNum =
      T.all (`elem` ("+-.0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ\
                     \_abcdefghijklmnopqrstuvwxyz" :: String))

-- | Read and parse NarInfo directly from file.
readNarFile :: FilePath -> IO (Either String NarInfo)
readNarFile = fmap Y.decode1Strict . B.readFile

-- | Parsing error message generation.
failWith :: Monad m => String -> Text -> m a
failWith desc tSrc =
-- If not using `fail` then what? `empty` does not include a custom message.
  fail $ "NarInfo YAML parsing Error! " ++ sentence ++ ":\n" ++ T.unpack tSrc
  where
    sentence = case desc of
                 (char:rest) -> C.toUpper char : rest
                 _ -> ""
