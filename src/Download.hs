{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Download where

import Network.HTTP.Req
import Network.HTTP.Req.Conduit (responseBodySource)
import Network.HTTP.Client (responseStatus, HttpExceptionContent(..))
import Network.HTTP.Types.Status (ok200)
import Conduit ((.|), runConduitRes, sinkHandle)
import Control.Exception (try)
import qualified Data.ByteString.Lazy as B
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory (renameFile)
import System.IO (Handle, hClose)
import System.IO.Temp (withTempFile)
import Control.Applicative (liftA2)
import Control.Arrow (left)
import Control.Monad (void)

import System.Nix.NarInfo


type UrlEndpoint = Text

data DownloadError = HttpError HttpException
                   | CheckError
                   | NarInfoError String
  deriving (Show)


defHost :: Url 'Https
defHost = https "cache.nixos.org"
-- defHost = https "httpbin.org"

-- | Downloads a file, checks and writes to a file system. Actually download is
-- a stream of http body to a temporary file. If the check is positive, the
-- temporary file is renamed to the `UrlEndpoint`.
downloadCheckAndSave :: FilePath -> (B.ByteString -> Bool) -> UrlEndpoint
                     -> IO (Either DownloadError B.ByteString)
downloadCheckAndSave path check urlEndPoint = withTempFile path template
  $ \fpTmp handle -> do
  -- I don't like how this looks. Monad Transformer needed?
  eDownload <- streamDownload urlEndPoint handle
  case eDownload of
    Right bs ->
      if check bs
      then do
        hClose handle
        -- `withTempFile` allows removing the temporary file inside the action
        renameFile fpTmp filepath
        Right <$> B.readFile filepath
      else return $ Left CheckError
    Left _ -> return eDownload
  where
    template = T.unpack $ T.takeWhileEnd (/= '/') urlEndPoint
    filename = T.unpack urlEndPoint -- (expect caveats)
    filepath = path ++ "/" ++ filename

-- | Downloads a http body to a file handle in a streaming fashion (in constant
-- memory). Any response code other than 200 is treated as `Left`
-- `HttpException`.
streamDownload :: UrlEndpoint -> Handle
               -> IO (Either DownloadError B.ByteString)
streamDownload urlEndPoint handle = left HttpError <$>
  liftA2 (<$) exposeFile tryStreamDownload
  where
    -- req config
    config = defaultHttpConfig {httpConfigCheckResponse = checkResponse}
    -- check HTTP response code
    checkResponse _ resp bs =
      if responseStatus resp == ok200
      then Nothing
      else Just $ StatusCodeException (void resp) bs -- :@
    -- the actual stream download to the filepath
    tryStreamDownload = try $ runReq config
      $ reqBr GET (defHost /: urlEndPoint) NoReqBody mempty $ \r ->
      runConduitRes $ responseBodySource r .| sinkHandle handle
    -- lazily read the downloaded file (from the handle).
    exposeFile = B.hGetContents handle

-- | Downloads a http body to a file handle in a streaming fashion (in constant
-- memory). Any response code other than 200 is treated as `Left`
-- `HttpException`.
streamDownload' :: UrlEndpoint -> Handle
               -> IO B.ByteString
streamDownload' urlEndPoint handle = doDownload >> exposeFile
  where
    -- req config
    config = defaultHttpConfig {httpConfigCheckResponse = checkResponse}
    -- check HTTP response code
    checkResponse _ resp bs =
      if responseStatus resp == ok200
      then Nothing
      else Just $ StatusCodeException (void resp) bs -- :@
    -- the actual stream download to the filepath
    doDownload = runReq config
      $ reqBr GET (defHost /: urlEndPoint) NoReqBody mempty $ \r ->
      runConduitRes $ responseBodySource r .| sinkHandle handle
    -- lazily read the downloaded file (from the handle).
    exposeFile = B.hGetContents handle

-- | Make `UrlEndpoint` for NarInfo from store-path.
mkNarInfoEndpFromStorePath :: Text -> Maybe UrlEndpoint
mkNarInfoEndpFromStorePath t = mkNarInfoEndpFromStoreHash <$> parseStorePath t

-- | Make `UrlEndpoint` for NarInfo from StoreHash (Reference).
mkNarInfoEndpFromStoreHash :: StoreHash -> UrlEndpoint
mkNarInfoEndpFromStoreHash = flip T.append ".narinfo"

test :: IO [Either DownloadError B.ByteString]
test = do
  storePaths <- T.lines <$> T.readFile "test-data/store-paths_"
  case traverse mkNarInfoEndpFromStorePath storePaths of
    Just narInfoNames ->
      mapM (downloadCheckAndSave "test-results" (const True)) narInfoNames
    Nothing -> error "invalid store-paths file"

test2 :: [Text] -> IO [Either DownloadError B.ByteString]
test2 storePaths = do
  case traverse parseStorePath storePaths of
    Just storeHashes ->
      mapM (downloadCheckAndSave "test-results" (const True)) storeHashes
    Nothing -> error "invalid store-paths file"

readStoreNames :: FilePath -> IO (Maybe [StoreName])
readStoreNames fp =
  traverse mkNarInfoEndpFromStorePath . T.lines <$> T.readFile fp

-- narInfoTreeToList :: StoreHash -> IO [StoreHash]
-- narInfoTreeToList = 

download :: UrlEndpoint -> IO (Either DownloadError B.ByteString)
download = downloadCheckAndSave "test-results" (const True)

-- treeToHashList :: NarInfo -> IO (Either DownloadError [NarHash])
-- treeToHashList (NarInfo {_fileHash=h, _references=rs}) = do
--   narInfosIO <- map download $ map mkNarInfoEndpFromStoreHash rs
--   return (Right h : )
--   sequence $
--   (Right h) : (concatMap treeToHashList rs)
--   where
--     download = downloadCheckAndSave "test-results" (const True)
--     downloadReference :: StoreHash -> IO (Either DownloadError B.ByteString)
