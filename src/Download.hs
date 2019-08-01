{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Download where

import Network.HTTP.Req
import Network.HTTP.Req.Conduit (responseBodySource)
import Network.HTTP.Client (Response, BodyReader)
import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString, toShort)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.Directory (doesFileExist, renameFile)
import System.IO (hClose)
import System.IO.Temp (withTempFile)
import Conduit
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.Set as Set
import Control.Monad (foldM)

import System.Nix.NarInfo
import qualified System.Nix.Base32 as NixBase32


type StorePath = Text
type UrlEndpoint = Text

data DownloadError = HttpError HttpException
                   | CheckError
                   | NarInfoError String
  deriving (Show)


defHost :: Url 'Https
defHost = https "cache.nixos.org"
-- defHost = https "httpbin.org"

defPath :: FilePath
defPath = "test-results"

myRespTimeout :: Option scheme
myRespTimeout = responseTimeout $ 60 * 1000000 -- 60 seconds

downloadWithBodyReader :: UrlEndpoint -> (Response BodyReader -> IO a) -> IO a
downloadWithBodyReader urlEndPoint bodyReader = runReq defaultHttpConfig
  $ reqBr GET (defHost /: urlEndPoint) NoReqBody myRespTimeout bodyReader

-- | Downloads a file if it hasn't been found in FS, checks and writes to a file
-- system. Actually download is a stream of http body to a temporary file. If
-- the check is positive, the temporary file is renamed to the `UrlEndpoint`.
-- Returns the downloaded `FilePath`. We assume that if a file is present it has
-- to be valid.
downloadCheckAndSave :: (Text -> Bool) -> UrlEndpoint -> IO FilePath
downloadCheckAndSave check urlEndpoint = do
  exists <- doesFileExist filepath
  if exists
    then return filepath
    else withTempFile defPath template
    $ \fpTmp hndl -> downloadWithBodyReader urlEndpoint $ bodyReader fpTmp hndl
  where
    template = T.unpack $ T.takeWhileEnd (/= '/') urlEndpoint
    filename = T.unpack urlEndpoint -- (expect caveats)
    filepath = defPath ++ "/" ++ filename
    bodyReader fpTmp hndl r = do
      hash <- runConduitRes $ responseBodySource r
              .| getZipSink (ZipSink (sinkHandle hndl) *> ZipSink sinkHash)
      if check $ NixBase32.encode hash
        then
        do
          hClose hndl
          -- `withTempFile` allows removing the temporary file inside the action
          renameFile fpTmp filepath
          return filepath
        else error $ "downloaded file ("
             ++ T.unpack urlEndpoint ++ ") checksum failure!"

-- | Make `UrlEndpoint` for NarInfo from store-path.
mkNarInfoEndpFromStorePath :: Text -> Maybe UrlEndpoint
mkNarInfoEndpFromStorePath t = mkNarInfoEndpFromStoreHash <$> parseStorePath t

-- | Make `UrlEndpoint` for NarInfo from StoreHash (Reference).
mkNarInfoEndpFromStoreHash :: StoreHash -> UrlEndpoint
mkNarInfoEndpFromStoreHash = flip T.append ".narinfo"

naiveRecurse :: Set.Set ShortByteString -> NarInfo
             -> IO ([NarInfo], Set.Set ShortByteString)
naiveRecurse hs n = do
  -- downloading only new NarInfos (missing in HashSet) this one references
  newFiles <-
    mapM (downloadCheckAndSave (const True) . mkNarInfoEndpFromStoreHash)
    (filter (not . flip Set.member hs . compactHash) $ _references n)
  newNarInfos <- mapM readNarFile newFiles
  (urls, hsNew) <-
    foldM niStep ([], Set.insert (compactHash $ _storeHash n) hs) newNarInfos
  return (n : urls, hsNew)
  where
    niStep (acc, hs') nn = do
      (urls, hsNew) <- naiveRecurse hs' nn
      return (urls ++ acc, hsNew)
    compactHash = toShort . T.encodeUtf8

downloadNixBinCacheForStorePaths' :: [StorePath] -> IO ()
downloadNixBinCacheForStorePaths' storePaths =
  -- parse store paths and make urls endpoints corresponding to NarInfos
      mapM (fmap mkNarInfoEndpFromStoreHash . parseStorePath) storePaths
  -- download `NarInfo` files corresponding to every store path
  >>= mapM (downloadCheckAndSave (const True))
  -- parse `NarInfo` files (of input store paths)
  >>= mapM readNarFile
  -- download all nested referenced `NarInfo`s
  >>= fmap fst . foldM niStep ([], Set.empty)
  -- download Nar files (and validate checksum on the fly) for every `NarInfo`
  >>= mapM_ (\n -> downloadCheckAndSave (== _fileHash n) (_url n))
  where
    niStep (acc, hs) narInfo = do
      putStr "taking store path: "; print $ _storeHash narInfo
      (ns, hsNew) <- naiveRecurse hs narInfo
      return (ns ++ acc, hsNew)

downloadNixBinCacheForStorePaths :: [StorePath] -> IO ()
downloadNixBinCacheForStorePaths storePaths =
  runConduit
    ( yieldMany storePaths
    .| iterMC (\line -> putStr "taking store path: " >> print line)
    -- parse store paths and make urls endpoints corresponding to NarInfos
    .| mapMC (fmap mkNarInfoEndpFromStoreHash . parseStorePath)
    -- download `NarInfo` files corresponding to every store path
    .| mapMC (downloadCheckAndSave (const True))
    -- parse `NarInfo` files (of input store paths)
    .| mapMC readNarFile
    -- download all nested referenced `NarInfo`s
    .| recurseAllNars Set.empty
    -- download Nar files (and validate checksum on the fly) for every `NarInfo`
    .| mapMC (\n -> downloadCheckAndSave (== _fileHash n) (_url n))
    .| printC
    )

-- | Recursively download all `NarInfo`s. Repeated downloads are avoided via
-- `Set` of cached store hashes.
recurseAllNars :: MonadIO m => Set.Set ShortByteString
               -> ConduitT NarInfo NarInfo m ()
recurseAllNars hs = do
  mNarInfo <- await
  case mNarInfo of
    Nothing -> return () -- the source exhausted
    Just narInfo -> do
      -- liftIO $ putStr "recurse: " >> print (_storeHash narInfo)
      yield narInfo
      -- downloading only new NarInfos (missing in HashSet) this one references
      refNarInfoFiles <- liftIO
        $ mapM (downloadCheckAndSave (const True) . mkNarInfoEndpFromStoreHash)
        (filter (not . flip Set.member hs . compactHash) $ _references narInfo)
      -- IO because of treating `Left` as `error`
      refNarInfos <- liftIO
        $ mapM readNarFile refNarInfoFiles
      -- push newly downloaded NarInfos back into stream (looks like a hack)
      mapM_ leftover refNarInfos
      -- yieldMany refNarInfos .| recurseAllNars
      -- recursive call for processing "leftovers" and the rest NarInfo stream
      recurseAllNars (Set.insert (compactHash $ _storeHash narInfo) hs)
  where
    compactHash = toShort . T.encodeUtf8

-- | A 'Sink' that hashes a stream of 'ByteString'@s@ and
-- creates a sha256 digest.
sinkHash :: Monad m => ConduitT ByteString Void m ByteString
sinkHash = sink SHA256.init
  where
    sink ctx = do
      b <- await
      case b of
        Nothing -> return $! SHA256.finalize ctx
        Just bs -> sink $! SHA256.update ctx bs
