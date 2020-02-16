{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Download.BinaryCache
  ( downloadBinCacheForStorePaths
  , downloadBinCacheForStorePaths', downloadBinCacheForStorePathsC
  , HashCache
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
import Data.Either (rights)

import Download.Common
import System.Nix.NarInfo as N
import qualified System.Nix.Base32 as NixBase32

type StorePath = Text
type UrlEndpoint = Text
type HashCache = Set.Set ShortByteString


defBcHost :: Url 'Https
defBcHost = https "cache.nixos.org"

-- | Download a file without checksum comparison.
downloadNoCheckE :: (MonadReader DownloadAppConfig m, MonadIO m)
                => UrlEndpoint -> m (Either HttpException ByteString)
downloadNoCheckE endp =
  downloadAndSave (Right (defBcHost /: endp, mempty)) (T.unpack endp)

-- | Download a file in a streaming manner, validating its checksum before
-- renaming from temporary.
download :: (MonadReader DownloadAppConfig m, MonadIO m)
         => NarInfo -> m FilePath
download ni = downloadCheckAndSave
  (Right (defBcHost /: endp, mempty)) (T.unpack endp) (sinkSha256, hash)
  >>= \case Left de -> error $ errMsg de
            Right fp -> return fp
  where
    endp = _url ni
    hash = NixBase32.decode $ _fileHash ni
    errMsg de =
      "error downloading binary cache derivation \"" ++ T.unpack endp ++ "\"\n"
      ++ show de

compactHash :: Text -> ShortByteString
compactHash = toShort . T.encodeUtf8

newStoreHashes :: HashCache -> NarInfo -> [Text]
newStoreHashes hs n =
  filter (not . flip Set.member hs . compactHash) $ _references n

getNarInfo :: (MonadReader DownloadAppConfig m, MonadIO m)
           => StoreHash -> m (Either String NarInfo)
getNarInfo storeHash = downloadNoCheckE (mkNarInfoEndpFromStoreHash storeHash)
  >>= \case Left he -> liftIO (logFailed he) >> return (Left (show he))
            Right bs -> liftIO $ Right <$> decodeThrow bs
  where
    logFailed he = do
      appendFile "storepaths-fail.log" ("/nix/store/" ++ show storeHash ++ "\n")
      print (show he ++ "\n")

recurseNars :: (MonadIO m, MonadReader DownloadAppConfig m)
            => ([NarInfo], HashCache) -> NarInfo -> m ([NarInfo], HashCache)
recurseNars (acc, hs') nn = do
  (ns, hsNew) <- recurseNar hs' nn
  return (ns ++ acc, hsNew)

recurseNar :: (MonadReader DownloadAppConfig m, MonadIO m)
           => HashCache -> NarInfo -> m ([NarInfo], HashCache)
recurseNar hs n =
  let hsCur = Set.insert (compactHash $ _storeHash n) hs
  in do
    -- download only new `NarInfo`s this one references, missing in HashCache
    newNarInfos <- rights <$> mapM getNarInfo (newStoreHashes hs n)
    -- recursively download all referenced `NarInfo`s
    (ns, hsNew) <- foldM recurseNars ([], hsCur) newNarInfos
    -- append the input `NarInfo` to the accumulator and return updated hash set
    return (n : ns, hsNew)

downloadBinCacheForStorePaths' :: (MonadReader DownloadAppConfig m, MonadIO m)
                              => HashCache -> [StorePath] -> m HashCache
downloadBinCacheForStorePaths' hs storePaths =
  -- parse store paths and make urls endpoints corresponding to NarInfos
      mapM parseStorePath storePaths
  -- download and decode `NarInfo` files corresponding to every store path
  >>= fmap rights . mapM getNarInfo
  -- download all nested referenced `NarInfo`s
  >>= foldM (\a n -> log' n >> recurseNars a n) ([], hs)
  -- download Nar files (and validate checksum on the fly) for every `NarInfo`
  >>= \(ns, hs') -> mapM_ download ns
  >> return hs'
  where
    log' n =
      liftIO $ putStrLn $ "taking target store path " ++ show (_storeHash n)

-- | Recursively download all `NarInfo`s. Repeated downloads are avoided via
-- `Set` of cached store hashes.
recurseAllNars :: (MonadReader DownloadAppConfig m, MonadIO m)
               => HashCache -> ConduitT NarInfo NarInfo m HashCache
recurseAllNars hs = do
  mNarInfo <- await
  case mNarInfo of
    Nothing -> return hs -- the source exhausted
    Just narInfo -> do
      yield narInfo
      -- download only new `NarInfo`s this one references, missing in HashCache
      refNarInfos <- rights <$> mapM getNarInfo (newStoreHashes hs narInfo)
      -- push newly downloaded `NarInfo`s back into stream (seems to be a hack!)
      mapM_ leftover refNarInfos
      -- recursive call for processing "leftovers" and the rest NarInfo stream
      recurseAllNars (Set.insert (compactHash $ _storeHash narInfo) hs)

downloadBinCacheForStorePathsC :: (MonadReader DownloadAppConfig m, MonadIO m)
                               => HashCache -> [StorePath] -> m HashCache
downloadBinCacheForStorePathsC hs storePaths =
  runConduit
    ( yieldMany storePaths
    .| iterMC -- logging
      (\line -> liftIO $ putStr "taking target store path: " >> print line)
    -- parse store paths and make urls endpoints corresponding to `NarInfo`s
    .| mapMC parseStorePath
    -- download and decode `NarInfo` files corresponding to every store path
    .| mapMC getNarInfo
    .| rightsC
    -- download all nested referenced `NarInfo`s
    .| recurseAllNars hs
    `fuseUpstream` mapMC download
    `fuseUpstream` printC
    )
  where
    rightsC :: Monad m => ConduitT (Either a b) b m ()
    rightsC = awaitForever $ either (\_ -> return ()) yield

-- | Download Nix binary cache for the given target `StorePath`s list and all
-- its recursive references (dependencies), automatically selecting `NarInfo`
-- recursion implementation based on `MonadReader DownloadAppConfig`.
downloadBinCacheForStorePaths :: (MonadReader DownloadAppConfig m, MonadIO m)
                              => HashCache -> [StorePath] -> m HashCache
downloadBinCacheForStorePaths hs storePaths =
  ask >>= \(DownloadAppConfig _ useConduit) -> bcDlFunc useConduit hs storePaths
 where
   bcDlFunc :: (MonadReader DownloadAppConfig m, MonadIO m)
            => Bool -> (HashCache -> [StorePath] -> m HashCache)
   bcDlFunc True  = downloadBinCacheForStorePathsC
   bcDlFunc False = downloadBinCacheForStorePaths'
