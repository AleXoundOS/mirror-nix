{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Download where

import Network.HTTP.Req
import Network.HTTP.Req.Conduit (responseBodySource)
import Network.HTTP.Client (Response, BodyReader)
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory (doesFileExist, renameFile)
import System.IO (hClose)
import System.IO.Temp (withTempFile)
import Conduit
import qualified Crypto.Hash.SHA256 as SHA256

import System.Nix.NarInfo
import qualified System.Nix.Base32 as NixBase32


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

downloadWithBodyReader :: UrlEndpoint -> (Response BodyReader -> IO a) -> IO a
downloadWithBodyReader urlEndPoint bodyReader = runReq defaultHttpConfig
  $ reqBr GET (defHost /: urlEndPoint) NoReqBody mempty bodyReader

-- | Downloads a file if it hasn't been found in FS, checks and writes to a file
-- system. Actually download is a stream of http body to a temporary file. If
-- the check is positive, the temporary file is renamed to the `UrlEndpoint`.
-- Returns the downloaded `FilePath`. We assume that if a file is present it has
-- to be valid.
downloadCheckAndSave :: UrlEndpoint -> (Text -> Bool) -> IO FilePath
downloadCheckAndSave urlEndpoint check = do
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
        else error "downloaded file checksum failure!"

-- | Make `UrlEndpoint` for NarInfo from store-path.
mkNarInfoEndpFromStorePath :: Text -> Maybe UrlEndpoint
mkNarInfoEndpFromStorePath t = mkNarInfoEndpFromStoreHash <$> parseStorePath t

-- | Make `UrlEndpoint` for NarInfo from StoreHash (Reference).
mkNarInfoEndpFromStoreHash :: StoreHash -> UrlEndpoint
mkNarInfoEndpFromStoreHash = flip T.append ".narinfo"

test3 :: IO [UrlEndpoint]
test3 = do
  storePathsLines <- T.lines <$> T.readFile "test-data/store-paths_"
  runConduit
    $ yieldMany storePathsLines
    .| mapMC (fmap mkNarInfoEndpFromStoreHash . parseStorePath)
    .| sinkList

readStoreNames :: FilePath -> IO (Maybe [StoreName])
readStoreNames fp =
  traverse mkNarInfoEndpFromStorePath . T.lines <$> T.readFile fp

-- narInfoTreeToList :: StoreHash -> IO [StoreHash]
-- narInfoTreeToList = 

-- treeToHashList :: NarInfo -> IO (Either DownloadError [NarHash])
-- treeToHashList (NarInfo {_fileHash=h, _references=rs}) = do
--   narInfosIO <- map download $ map mkNarInfoEndpFromStoreHash rs
--   return (Right h : )
--   sequence $
--   (Right h) : (concatMap treeToHashList rs)
--   where
--     download = downloadCheckAndSave "test-results" (const True)
--     downloadReference :: StoreHash -> IO (Either DownloadError B.ByteString)

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
