{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Download.FixedOutputs
  ( downloadFixedOutputDerivations
  , downloadFixedOutputTarball
  ) where

import Data.Text (Text)
import qualified Data.Text as T (unpack)
import Data.Monoid ((<>))
import Control.Monad.Reader
import qualified Data.Text.Encoding as T (encodeUtf8)
import Data.Maybe (mapMaybe)
import Network.HTTP.Req

import System.Nix.FixedOutput
import qualified System.Nix.Base32 as NixBase32
import Download.Common
import Download.BinaryCache


-- | For fixed output sources, i.e. "tarballs", of derivations.
defTbHost :: Url 'Https
defTbHost = https "tarballs.nixos.org"

-- | List of `Request`s the fixed output "tarball" can be downloaded from. First
-- comes the nixos "tarballs" mirror request. Throws an exception on a malformed
-- url.
getTarballsUrls :: FixedOutputInfo -> [Request]
getTarballsUrls foi = Right (tarballsMirrorUrl, mempty) : urls
  where
    tarballsMirrorUrl =
      defTbHost /: hashTypeTxt (_type foi) /: _hash foi
    -- we fail here at "ftp://"
    -- urls = map (fromJust . parseUrl . T.encodeUtf8) $ _urls foi
    urls = mapMaybe (parseUrl . T.encodeUtf8) $ _urls foi

-- | Make a `Request` fixed output derivation can be downloaded from
-- (cache.nixos.org). Throws exception on faulty store-path hash.
-- mkFoDrvReq :: FixedOutputInfo -> Request
-- mkFoDrvReq foi = Right (defBcHost /: getFoEndpoint foi, mempty)

-- getFoNarInfoEndp :: FixedOutputInfo -> Text
-- getFoNarInfoEndp = fromJust . mkNarInfoEndpFromStorePath . _path

-- | Textual representation of a hash type.
hashTypeTxt :: HashType -> Text
hashTypeTxt HashTypeSha1   = "sha1"
hashTypeTxt HashTypeSha256 = "sha256"
hashTypeTxt HashTypeSha512 = "sha512"

-- | Conduit sink that computes the specified hash type.
sinkOfhashType :: HashType -> HashSink
sinkOfhashType HashTypeSha1   = sinkSha1
sinkOfhashType HashTypeSha256 = sinkSha256
sinkOfhashType HashTypeSha512 = sinkSha512

-- | Download a fixed output "tarball". Returns `Right` `FilePath` on success,
-- `Left` `[DownloadError]` otherwise.
downloadFixedOutputTarball :: (MonadReader DownloadAppConfig m, MonadIO m)
  -- TODO NonEmpty List for errors?
  => FixedOutputInfo -> m (Maybe FilePath, [DownloadError])
downloadFixedOutputTarball foi =
  let go :: (MonadReader DownloadAppConfig m, MonadIO m)
         => [DownloadError] -> [Request] -> m (Maybe FilePath, [DownloadError])
      go es (r:rs) = downloadCheckAndSave r filename (sink, hash)
        >>= \case Left e -> go (e:es) rs
                  Right fp -> return (Just fp, es)
      go es [] = return (Nothing, es)
      -- conduit sink, computing the checksum
      sink = sinkOfhashType hashType
      hash = NixBase32.decode $ _hash foi
      hashType = _type foi
      filename = T.unpack $ hashTypeTxt hashType <> "/" <> _hash foi
  in go [] $ getTarballsUrls foi

-- | Download fixed output derivation (as opposed to "tarball") with all
-- referenced derivations recursively.
downloadFixedOutputDerivations :: (MonadReader DownloadAppConfig m, MonadIO m)
  => HashCache -> [FixedOutputInfo] -> m HashCache
downloadFixedOutputDerivations hc fs =
  downloadBinCacheForStorePaths hc $ map _path fs

-- downloadFixedOutputs :: (MonadReader DownloadAppConfig m, MonadIO m)
--   => HashCache -> [FixedOutputInfo] -> m HashCache
-- downloadFixedOutputs hc fs =
--   let downloadTask :: ([Request], [StorePath])
--       downloadTask = foldr 
