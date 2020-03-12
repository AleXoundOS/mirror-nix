{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Download.Nix.Common
  ( DownloadAppConfig(..), DownloadError(..), Request, HashSink
  , downloadCheckAndSave, downloadCheckAndSave'
  , downloadAndSave, downloadAndSave'
  , reqOrExcept
  , sinkHash, sinkSha1, sinkSha256, sinkSha512, sinkBypass
  , sha1, sha256, sha512
  ) where

import Network.HTTP.Req
import Network.HTTP.Req.Conduit (responseBodySource)
import Network.HTTP.Client (Response, BodyReader)
import Data.ByteString (ByteString, readFile, writeFile)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T (encodeUtf8)
import System.FilePath.Posix (takeDirectory)
import System.Directory (createDirectoryIfMissing, doesFileExist, renameFile)
import System.IO (hClose)
import System.IO.Temp (withTempFile)
import Conduit
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Crypto.Hash.SHA512 as SHA512
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Control.Monad.Reader
import qualified Data.ByteString.Base16 as Base16 (encode)
import Control.Exception.Safe


data DownloadAppConfig = DownloadAppConfig
                         { _basePath :: FilePath, _useConduit :: Bool }
  deriving (Show)

type Request = Either (Url 'Http, Option 'Http) (Url 'Https, Option 'Https)
type HashSink = ConduitT ByteString Void (ResourceT IO) ByteString

data DownloadError = HttpError HttpException
                   | CheckError (ByteString, ByteString) FilePath

data HashFuncs c = HashFuncs
  { _ctx      :: c
  , _update   :: c -> ByteString -> c
  , _finalize :: c -> ByteString
  }


-- is it a bad thing to do?
instance Show DownloadError where
  show (HttpError he) = show he
  show (CheckError (csExp, csGot) fp) =
    "Downloaded file (" ++ fp ++ ") checksum validation failure!\n" ++
    "Expected: " ++ show (Base16.encode csExp) ++ "\n" ++
    "Got:      " ++ show (Base16.encode csGot)


sha1 :: HashFuncs SHA1.Ctx
sha1 = HashFuncs SHA1.init SHA1.update SHA1.finalize

sha256 :: HashFuncs SHA256.Ctx
sha256 = HashFuncs SHA256.init SHA256.update SHA256.finalize

sha512 :: HashFuncs SHA512.Ctx
sha512 = HashFuncs SHA512.init SHA512.update SHA512.finalize

sinkSha1 :: HashSink
sinkSha1 = sinkHash sha1

sinkSha256 :: HashSink
sinkSha256 = sinkHash sha256

sinkSha512 :: HashSink
sinkSha512 = sinkHash sha512

myRespTimeout :: Option scheme
myRespTimeout = responseTimeout $ 60 * 1000000  -- ^ 60 seconds

-- | GET an HTTP body. Returns entire HTTP body as `Right` strict `ByteString`.
-- `HttpException`s are treated as `Left`.
get :: Request -> IO (Either HttpException ByteString)
get r =
  -- FIXME: can it be better???? wtf?
  case r of
    (Left x) -> doReq x
    (Right x) -> doReq x
  where
    doReq (url, options) = handle handler
      $ fmap (Right . responseBody)
      $ runReq defaultHttpConfig
      $ req GET url NoReqBody bsResponse (options <> myRespTimeout)
    -- | Catch only `HttpException`.
    handler :: HttpException -> IO (Either HttpException ByteString)
    handler = return . Left

getWithBodyReader :: Request -> (Response BodyReader -> IO a)
                  -> IO (Either HttpException a)
getWithBodyReader r bodyReader =
  -- FIXME: can it be better???? wtf?
  case r of
    (Left x) -> doReq x
    (Right x) -> doReq x
  where
    doReq (url, options) = handle handler $ fmap Right
      $ runReq defaultHttpConfig
      $ reqBr GET url NoReqBody (options <> myRespTimeout) bodyReader
      -- | Catch only `HttpException`.
    handler :: HttpException -> IO (Either HttpException a)
    handler = return . Left

reqOrExcept :: Text -> Request
reqOrExcept t = fromMaybe
  (error $ "cannot parse url: " ++ T.unpack t)
  -- Why does it work with `Request` being not polymorphic??????
  -- What if we get (Url 'Https) (Option 'Http)???
  (parseUrl $ T.encodeUtf8 t)

-- | Downloads a file if it hasn't been found in FS, checks and writes to a file
-- system. Download is a stream of HTTP body to a temporary file, computing the
-- checksum on the fly. If the checksum matches, the file is renamed to the
-- given name. Returns the resulting `FilePath` as `Right`. Otherwise, it's put
-- into special "bad-checksum" dir. It's assumed that if a file is present it
-- has to be valid, hence download is skipped.
downloadCheckAndSave :: (MonadReader DownloadAppConfig m, MonadIO m)
  => Request -> FilePath
  -> (ConduitT ByteString Void (ResourceT IO) ByteString, ByteString)
  -> m (Either DownloadError FilePath)
downloadCheckAndSave request filename (sink, hash1) = do
  (DownloadAppConfig basePath _) <- ask
  let filepath = basePath ++ "/" ++ filename
      filepathBadCS = basePath ++ ".bad-checksum/" ++ filename
  exists <- liftIO $ doesFileExist filepath
  if exists
    then return (Right filepath)
    else liftIO
         $ withTempFile basePath tmpFileTemplate
         $ \fpTmp hndl ->
             do
               dlRes <- getWithBodyReader request $ bodyReader hndl
               case dlRes of
                 Left httpExcept -> return $ Left (HttpError httpExcept)
                 Right hash2 ->
                   condRename (hash1 == hash2) fpTmp
                   (filepathBadCS, Left . CheckError (hash1, hash2))
                   (filepath, Right)
  where
    tmpFileTemplate = T.unpack $ T.takeWhileEnd (/= '/') $ T.pack filename
    bodyReader hndl r = do
      hash <- runConduitRes
        $ responseBodySource r
        .| getZipSink (ZipSink (sinkHandle hndl) *> ZipSink sink)
      hClose hndl
      return hash
    condRename bool src (dstFalse, useFalse) (dstTrue, useTrue) =
      let (dst, use) = if bool
                       then (dstTrue, useTrue)
                       else (dstFalse, useFalse)
      in do
        createDirectoryIfMissing True $ takeDirectory dst
        -- | `withTempFile` allows removing the temporary file inside the action
        renameFile src dst
        return $ use dst

-- | Same as `downloadCheckAndSave`, except throws exception on `DownloadError`
-- with msg.
downloadCheckAndSave' :: (MonadReader DownloadAppConfig m, MonadIO m)
  => Request -> FilePath
  -> (ConduitT ByteString Void (ResourceT IO) ByteString, ByteString)
  -> (DownloadError -> String)
  -> m FilePath
downloadCheckAndSave' r f (s, h) mkErrMsg = downloadCheckAndSave r f (s, h)
  >>= \case Left de -> error $ mkErrMsg de
            Right fp -> return fp

-- | Download (GET) a file and write to FS (without checksum validation).
-- Returns entire HTTP body as `Right` strict `ByteString`. Throws an exception
-- if file exists. `HttpException`s are treated as `Left`.
downloadAndSave :: (MonadReader DownloadAppConfig m, MonadIO m)
                => Request -> FilePath
                -> m (Either HttpException ByteString)
downloadAndSave request filename = do
  (DownloadAppConfig basePath _) <- ask
  let filepath = basePath ++ "/" ++ filename
  exists <- liftIO $ doesFileExist filepath
  if exists
    then Right <$> liftIO (Data.ByteString.readFile filepath)
    else liftIO $ get request
         >>= \eRes ->
               case eRes of
                 Left _he -> return ()
                 Right bs -> Data.ByteString.writeFile filepath bs
               >> return eRes

downloadAndSave' :: (MonadReader DownloadAppConfig m, MonadIO m)
                 => Request -> FilePath -> m ByteString
downloadAndSave' r f =
  either (error . mkErrMsg) id <$> downloadAndSave r f
  where
    mkErrMsg he = error
      $ "exception while downloading `" ++ urlShow ++
      "` to \"" ++ f ++ "\"\n" ++ show he ++ "`"
    urlShow = case r of
                Left (url, _) -> show url
                Right (url, _) -> show url

-- | A 'Sink' that hashes a stream of 'ByteString'@s@ and creates a digest.
sinkHash :: Monad m => HashFuncs a -> ConduitT ByteString Void m ByteString
sinkHash (HashFuncs init' update finalize) = sink init'
  where
    sink ctx = do
      b <- await
      case b of
        Nothing -> return $! finalize ctx
        Just bs -> sink $! update ctx bs

sinkBypass :: Monad m => ConduitT ByteString Void m ByteString
sinkBypass = return $! mempty
