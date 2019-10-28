{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Download.BinaryCache
  ( downloadBinCacheForStorePaths, downloadBinCacheForStorePaths'
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString, toShort)
import qualified Data.Set as Set
import Control.Monad (foldM)
import Conduit
import Control.Monad.Reader
import Network.HTTP.Req

import Download.Common
import System.Nix.NarInfo as N
import qualified System.Nix.Base32 as NixBase32

type StorePath = Text
type UrlEndpoint = Text


defBcHost :: Url 'Https
defBcHost = https "cache.nixos.org"

-- | Download a file without checksum comparison. Returns strict `ByteString`.
downloadNoCheck :: (MonadReader DownloadAppConfig m, MonadIO m)
                => UrlEndpoint -> m ByteString
downloadNoCheck endp =
  downloadAndSave' (Right (defBcHost /: endp, mempty)) (T.unpack endp)

-- | Download a file in a streaming manner, validating its checksum before
-- renaming from temporary.
download :: (MonadReader DownloadAppConfig m, MonadIO m)
         => NarInfo -> m FilePath
download ni = downloadCheckAndSave'
  (Right (defBcHost /: endp, mempty)) (T.unpack endp) (sinkSha256, hash) errMsg
  where
    endp = _url ni
    hash = NixBase32.decode $ _fileHash ni
    errMsg de = show de -- TODO

compactHash :: Text -> ShortByteString
compactHash = toShort . T.encodeUtf8

newStoreHashes :: Set.Set ShortByteString -> NarInfo -> [Text]
newStoreHashes hs n =
  filter (not . flip Set.member hs . compactHash) $ _references n

getNarInfo :: (MonadReader DownloadAppConfig m, MonadIO m)
           => StoreHash -> m NarInfo
getNarInfo =
  ((liftIO . decodeThrow) =<<) . downloadNoCheck . mkNarInfoEndpFromStoreHash

recurseNars :: (MonadIO m, MonadReader DownloadAppConfig m)
            => ([NarInfo], Set.Set ShortByteString)
            -> NarInfo -> m ([NarInfo], Set.Set ShortByteString)
recurseNars (acc, hs') nn = do
  (ns, hsNew) <- recurseNar hs' nn
  return (ns ++ acc, hsNew)

recurseNar :: (MonadReader DownloadAppConfig m, MonadIO m)
  => Set.Set ShortByteString -> NarInfo
  -> m ([NarInfo], Set.Set ShortByteString)
recurseNar hs n =
  let hsCur = Set.insert (compactHash $ _storeHash n) hs
  in do
    -- download only new `NarInfo`s this one references and missing in HashSet
    newNarInfos <- mapM getNarInfo $ newStoreHashes hs n
    -- recursively download all referenced `NarInfo`s
    (ns, hsNew) <- foldM recurseNars ([], hsCur) newNarInfos
    -- append the input `NarInfo` to the accumulator and return updated hash set
    return (n : ns, hsNew)

downloadBinCacheForStorePaths' :: (MonadReader DownloadAppConfig m, MonadIO m)
                               => [StorePath] -> m ()
downloadBinCacheForStorePaths' storePaths =
  -- parse store paths and make urls endpoints corresponding to NarInfos
      mapM mkNarInfoEndpFromStorePath storePaths
  -- download and decode `NarInfo` files corresponding to every store path
  >>= mapM (((liftIO . decodeThrow) =<<) . downloadNoCheck)
  -- download all nested referenced `NarInfo`s
  >>= fmap fst . foldM (\a n -> debug n >> recurseNars a n) ([], Set.empty)
  -- download Nar files (and validate checksum on the fly) for every `NarInfo`
  >>= mapM_ download
  where
    debug nn = liftIO $ putStr $ "taking store path: " ++ show (_storeHash nn)
      -- recurseNars (acc, hs) nn
      -- (ns, hsNew) <- recurseAllNars' hs narInfo
      -- return (ns ++ acc, hsNew)

-- | Recursively download all `NarInfo`s. Repeated downloads are avoided via
-- `Set` of cached store hashes.
recurseAllNars :: (MonadReader DownloadAppConfig m, MonadIO m)
               => Set.Set ShortByteString -> ConduitT NarInfo NarInfo m ()
recurseAllNars hs = do
  mNarInfo <- await
  case mNarInfo of
    Nothing -> return () -- the source exhausted
    Just narInfo -> do
      yield narInfo
      -- download only new `NarInfo`s this one references and missing in HashSet
      refNarInfos <- mapM getNarInfo $ newStoreHashes hs narInfo
      -- push newly downloaded `NarInfo`s back into stream (seems to be a hack!)
      mapM_ leftover refNarInfos
      -- recursive call for processing "leftovers" and the rest NarInfo stream
      recurseAllNars (Set.insert (compactHash $ _storeHash narInfo) hs)

downloadBinCacheForStorePaths :: (MonadReader DownloadAppConfig m, MonadIO m)
                              => [StorePath] -> m ()
downloadBinCacheForStorePaths storePaths =
  runConduit
    ( yieldMany storePaths
    .| iterMC (\line -> liftIO $ putStr "taking store path: " >> print line)
    -- parse store paths and make urls endpoints corresponding to `NarInfo`s
    .| mapMC (fmap mkNarInfoEndpFromStoreHash . parseStorePath)
    -- download and decode `NarInfo` files corresponding to every store path
    .| mapMC (((liftIO . decodeThrow) =<<) . downloadNoCheck)
    -- parse `NarInfo` files (of input store paths)
    .| mapMC (liftIO . readNarFile)
    -- download all nested referenced `NarInfo`s
    .| recurseAllNars Set.empty
    -- download Nar files (and validate checksum on the fly) for every `NarInfo`
    .| mapMC download
    .| printC
    )
