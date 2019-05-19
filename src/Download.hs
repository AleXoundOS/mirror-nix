{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Download where

import Network.HTTP.Req
import Network.HTTP.Req.Conduit (responseBodySource)
import Network.HTTP.Client (responseStatus, HttpExceptionContent(..))
import Network.HTTP.Types.Status (ok200)
import Conduit ((.|), runConduitRes, sinkFileCautious)
import Control.Exception (try)
import qualified Data.ByteString.Lazy as B
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (renameFile)


defHost :: Url 'Https
defHost = https "cache.nixos.org"
-- defHost = https "httpbin.org"

-- | Downloads a file and writes to file system in a streaming fashion (in
-- constant memory). Http exceptions are treated as `Left`. File write
-- exceptions are not caught.
-- TODO whether to perform checksum control or save to a temporary destination?
downloadToFs :: Text -> FilePath -> (B.ByteString -> Bool)
             -> IO (Either HttpException B.ByteString)
downloadToFs urlEndPoint path check
  = exposeFile =<< checkExcept <$> tryStreamDownload
  where
    -- req config
    config = defaultHttpConfig {httpConfigCheckResponse = checkResponse}
    -- destination FilePath in FS
    filepath = path ++ "/" ++ T.unpack urlEndPoint
    -- the actual stream download to the filepath
    tryStreamDownload = try $ runReq config
      $ reqBr GET (defHost /: urlEndPoint) NoReqBody mempty $ \r ->
      runConduitRes $ responseBodySource r .| sinkFileCautious filepath
    -- check HTTP response code
    checkResponse _ resp bs =
      if responseStatus resp == ok200
      then Nothing
      else Just $ StatusCodeException (fmap (const ()) resp) bs -- :@
    -- | check specifically for HttpException and Maybe pass it further
    checkExcept :: Either HttpException () -> Maybe HttpException
    checkExcept (Left e) = Just e
    checkExcept _ = Nothing
    -- | lazily read the downloaded file and return if no HttpException
    exposeFile :: Maybe HttpException -> IO (Either HttpException B.ByteString)
    exposeFile (Just e) = return (Left e)
    exposeFile Nothing = B.readFile filepath >>= \bs -> return (Right bs)
