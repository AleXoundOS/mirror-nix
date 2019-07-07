{-# LANGUAGE OverloadedStrings #-}

module System.Nix.NarInfo where

import qualified Data.Text as T
import Data.Text (Text)
import Data.Yaml as Y
import qualified Data.Char as C

-- Data.Functor from ghc-8.2.2.
(<&>) :: Functor f => f a -> (a -> b) -> f b
as <&> f = f <$> as

infixl 1 <&>

data NarInfo = NarInfo
  { _storeHash   :: !StoreHash
  , _url         :: !Text  -- ^ nar file url compressed or uncompressed
  , _compression :: !NarCompressionType -- ^ compression type: bz2, xz, none
  , _fileHash    :: !FileHash  -- ^ sha256 of nar file compressed or not
  , _fileSize    :: Int
  , _narHash     :: Text   -- ^ uncompressed nar file hash
  , _narSize     :: Int
  , _references  :: ![StoreHash]  -- ^ store hashes this references (depends)
  , _deriver     :: Maybe Text
  , _sig         :: Text
  } deriving (Eq, Show)

type StoreName = Text
type StoreHash = Text
type FileHash = Text

-- | Types of compression supported for NAR archives.
data NarCompressionType = CompBz2 | CompXz | CompNone
  deriving (Eq, Show)

instance FromJSON NarInfo where
  parseJSON (Object o) = NarInfo
    <$> (parseStorePath =<< o .: "StorePath")
    <*> o .: "URL"
    <*> (parseNarComp   =<< o .: "Compression")
    <*> (parseFileHash  =<< o .: "FileHash")
    <*> o .: "FileSize"
    <*> o .: "NarHash"
    <*> o .: "NarSize"
    <*> (parseRefs      =<< o .:? "References") -- optional
    <*> o .:? "Deriver"
    <*> o .: "Sig"
    <&> fixNarInfo
  parseJSON x =
    fail $ "NarInfo YAML parsing Error! \
           \Given ByteString does not begin with YAML map:\n" ++ show x

-- Filter out references to itself.
fixNarInfo :: NarInfo -> NarInfo
fixNarInfo n = n {_references = filter (/= _storeHash n) $ _references n}

parseNarComp :: Monad m => Text -> m NarCompressionType
parseNarComp "xz" = pure CompXz
parseNarComp t = failWith "`Compression` type read from Narinfo is not `xz`" t

parseFileHash :: Monad m => Text -> m FileHash
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
readNarFile :: FilePath -> IO NarInfo
readNarFile = decodeFileThrow

-- | Parsing error message generation.
failWith :: Monad m => String -> Text -> m a
failWith desc tSrc =
-- If not using `fail` then what? `empty` does not include a custom message.
  fail $ "NarInfo YAML parsing Error! " ++ sentence ++ ":\n" ++ T.unpack tSrc
  where
    sentence = case desc of
                 (char:rest) -> C.toUpper char : rest
                 _ -> ""
