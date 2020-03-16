{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Download.Nix.BinaryCache
  ( downloadBinCacheForStoreNames', downloadBinCacheForStoreNamesC
  , downloadCacheForStoreName', downloadCacheForStoreNameC
  , HashCache
  , recurseNarInfo, recurseNarInfoAcc, recurseNarInfosB
  , recDlStoreNameNarInfos
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString, toShort)
import qualified Data.Set as Set
import Control.Monad (foldM, (<=<))
import Conduit
import Control.Monad.Reader
import Network.HTTP.Req

import Download.Nix.Common
import Download.Nix.Nars
import System.Nix.NarInfo as N
import System.Nix.StoreNames
import Utils


type UrlEndpoint = Text
type HashCache = Set.Set ShortByteString


defBcHost :: Url 'Https
defBcHost = https "cache.nixos.org"

-- | Download a file via https without checksum comparison.
downloadNoCheckE :: (MonadReader DownloadAppConfig m, MonadIO m)
                 => UrlEndpoint -> m (Either HttpException ByteString)
downloadNoCheckE endp =
  downloadAndSave (Right (defBcHost /: endp, mempty)) (T.unpack endp)

-- | Download a file in a streaming manner, validating its checksum before
-- renaming from temporary.
downloadNar' :: (MonadReader DownloadAppConfig m, MonadIO m)
             => NarInfo -> m FilePath
downloadNar' ni = eitherToError <$> downloadNar ni
  where
    eitherToError (Left de) = error $ errMsg de
    eitherToError (Right fp) = fp
    endp = _url ni
    errMsg de =
      "error downloading binary cache derivation \"" ++ T.unpack endp ++ "\"\n"
      ++ showDownloadError de

compactStoreHash :: StoreName -> ShortByteString
compactStoreHash = toShort . T.encodeUtf8 . storeNameHash

newStoreNames :: HashCache -> NarInfo -> [StoreName]
newStoreNames hs =
  filter (not . (`Set.member` hs) . compactStoreHash) . _references

getNarInfo :: (MonadReader DownloadAppConfig m, MonadIO m)
           => StoreName -> m (Either String NarInfo)
getNarInfo storeName = downloadNoCheckE (mkNarInfoEndpFromStoreName storeName)
  >>= \case Left  he -> liftIO (logFailed he) >> return (Left (show he))
            Right bs -> liftIO $ Right <$> decodeThrow bs
  where
    storeNameStr = T.unpack $ textStoreName storeName
    logFailed he =
      appendFile "narinfo-dl-except.log" $
        "/nix/store/" ++ storeNameStr ++ "\n" ++ show he ++ "\n\n"

-- | Is it depth-first variant?
recurseNarInfoAcc :: (MonadIO m, MonadReader DownloadAppConfig m)
  => ([NarInfo], HashCache) -> NarInfo -> m ([NarInfo], HashCache)
recurseNarInfoAcc (acc, hs') nn = do
  (ns, hsNew) <- recurseNarInfo hs' nn
  return (ns ++ acc, hsNew)

recurseNarInfo :: (MonadReader DownloadAppConfig m, MonadIO m)
               => HashCache -> NarInfo -> m ([NarInfo], HashCache)
recurseNarInfo hs n =
  let hsCur = Set.insert (compactStoreHash $ _storeName n) hs
  in do
    -- download only new `NarInfo`s this one references, missing in HashCache
    refNarInfos <- map forceEitherStr <$> mapM getNarInfo (newStoreNames hs n)
    -- recursively download all referenced `NarInfo`s
    (ns, hsNew) <- foldM recurseNarInfoAcc ([], hsCur) refNarInfos
    -- append the input `NarInfo` to the accumulator and return updated hash set
    return (n : ns, hsNew)

-- | Is it breadth-first variant?
recurseNarInfosB :: (MonadIO m, MonadReader DownloadAppConfig m)
  => ([NarInfo], HashCache) -> [NarInfo]
  -> m ([NarInfo], HashCache)
recurseNarInfosB (nsAcc, hsAcc) ns = do
  -- download only new `NarInfo`s given ones reference, missing in HashCache
  (refNarInfos, hsNew) <- foldM dlNewAcc ([], hsAcc) ns
  -- recursively download all referenced `NarInfo`s
  recurseNarInfosB (refNarInfos ++ nsAcc, hsNew) refNarInfos

dlNewAcc :: (MonadIO m, MonadReader DownloadAppConfig m)
         => ([NarInfo], HashCache) -> NarInfo -> m ([NarInfo], HashCache)
dlNewAcc (nsAcc, hsAcc) n = do
  nsNew <- map forceEitherStr <$> mapM getNarInfo (newStoreNames hsAcc n)
  return ( nsNew ++ nsAcc
         , Set.union hsAcc
           $ Set.fromList $ map (compactStoreHash . _storeName) nsNew )

downloadBinCacheForStoreNames' :: (MonadReader DownloadAppConfig m, MonadIO m)
                               => HashCache -> [StoreName] -> m HashCache
downloadBinCacheForStoreNames' hs storeNames =
  -- download and decode `NarInfo` files corresponding to every store path
  map forceEitherStr <$> mapM getNarInfo storeNames
  -- download all nested referenced `NarInfo`s
  >>= foldM (\a n -> log' n >> recurseNarInfoAcc a n) ([], hs)
  -- download Nar files (and validate checksum on the fly) for every `NarInfo`
  >>= \(ns, hs') -> mapM_ downloadNar ns
  >> return hs'
  where
    log' n = liftIO $ putStrLn
      $ "taking target store path " ++ showStoreNamePath (_storeName n)

recDlStoreNameNarInfos :: (MonadReader DownloadAppConfig m, MonadIO m)
  => ([NarInfo], HashCache) -> StoreName
  -> m (Either String ([NarInfo], HashCache))
recDlStoreNameNarInfos acc =
  sequenceA . (recurseNarInfoAcc acc <$>) <=< getNarInfo

-- | Recursively download all `NarInfo`s. Repeated downloads are avoided via
-- `Set` of cached store hashes.
recurseNarInfosC :: (MonadReader DownloadAppConfig m, MonadIO m)
                 => HashCache -> ConduitT NarInfo NarInfo m HashCache
recurseNarInfosC hs = do
  mNarInfo <- await
  case mNarInfo of
    Nothing -> return hs -- the source exhausted
    Just narInfo -> do
      yield narInfo
      -- download only new `NarInfo`s this one references, missing in HashCache
      refNarInfos <-
        map forceEitherStr <$> mapM getNarInfo (newStoreNames hs narInfo)
      -- push newly downloaded `NarInfo`s back into stream (seems to be a hack!)
      mapM_ leftover refNarInfos
      -- recursive call for processing "leftovers" and the rest NarInfo stream
      recurseNarInfosC (Set.insert (compactStoreHash $ _storeName narInfo) hs)

downloadBinCacheForStoreNamesC :: (MonadReader DownloadAppConfig m, MonadIO m)
                               => HashCache -> [StoreName] -> m HashCache
downloadBinCacheForStoreNamesC hs storeNames =
  runConduit
    ( yieldMany storeNames
    -- logging
    .| iterMC log'
    -- download and decode `NarInfo` files corresponding to every store name
    .| mapMC getNarInfo
    .| rightsC -- TODO: BAD
    -- download all nested referenced `NarInfo`s
    .| recurseNarInfosC hs
    `fuseUpstream` mapMC downloadNar
    `fuseUpstream` printC
    )
  where
    rightsC :: Monad m => ConduitT (Either a b) b m ()
    rightsC = awaitForever $ either (\_ -> return ()) yield
    log' storeName = liftIO
      $ putStrLn $ "taking target store path: " ++ showStoreNamePath storeName

-- | Download Nix binary cache for the given target `StorePath`s list and all
-- its recursive references (dependencies), selecting `NarInfo` recursion
-- implementation based on `MonadReader DownloadAppConfig`.
-- downloadBinCacheForStoreNames :: (MonadReader DownloadAppConfig m, MonadIO m)
--                               => HashCache -> [StoreName] -> m HashCache
-- downloadBinCacheForStoreNames hs storeNames =
--   asks appUseConduit >>= \useConduit -> bcDlFunc useConduit hs storeNames
--  where
--    bcDlFunc :: (MonadReader DownloadAppConfig m, MonadIO m)
--             => Bool -> (HashCache -> [StoreName] -> m HashCache)
--    bcDlFunc True  = downloadBinCacheForStoreNamesC
--    bcDlFunc False = downloadBinCacheForStoreNames'

-- downloadCacheForStoreName :: (MonadReader DownloadAppConfig m, MonadIO m)
--                           => HashCache -> StoreName -> m (Maybe HashCache)
-- downloadCacheForStoreName hs storeName =
--   asks appUseConduit >>= \useConduit -> bcDlFunc useConduit hs storeName
--  where
--    bcDlFunc True  = downloadCacheForStoreNameC
--    bcDlFunc False = downloadCacheForStoreName'

downloadCacheForStoreName' :: (MonadReader DownloadAppConfig m, MonadIO m)
                           => HashCache -> StoreName -> m (Maybe HashCache)
downloadCacheForStoreName' hs storeName = do
  eNarInfo <- getNarInfo storeName
  case eNarInfo of
    Left _errStr -> return Nothing
    Right narInfo -> do
      (ns, hs') <- recurseNarInfo hs narInfo
      mapM_ downloadNar ns
      return (Just hs')

downloadCacheForStoreNameC :: (MonadReader DownloadAppConfig m, MonadIO m)
                           => HashCache -> StoreName -> m (Maybe HashCache)
downloadCacheForStoreNameC hs storeName = do
  eNarInfo <- getNarInfo storeName
  case eNarInfo of
    Left _errStr -> return Nothing
    Right narInfo -> Just <$>
      runConduit
      ( yield narInfo
        -- download all nested referenced `NarInfo`s
        .| recurseNarInfosC hs
        `fuseUpstream` mapMC downloadNar
        `fuseUpstream` printC
      )
