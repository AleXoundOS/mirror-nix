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


defHost :: Url 'Https
defHost = https "cache.nixos.org"
-- defHost = https "httpbin.org"

-- | Downloads a file and writes to file system in a streaming fashion (in
-- constant memory). Http exceptions are treated as `Left`. File write
-- exceptions are not caught.
downloadToFs :: Text -> FilePath -> IO (Either HttpException B.ByteString)
downloadToFs urlEndPoint path = exposeFile =<< checkExcept <$> tryStreamDownload
  where
    config = defaultHttpConfig {httpConfigCheckResponse = checkResponse}
    filepath = path ++ "/" ++ T.unpack urlEndPoint
    tryStreamDownload = try $ runReq config
      $ reqBr GET (defHost /: urlEndPoint) NoReqBody mempty $ \r ->
      runConduitRes $ responseBodySource r .| sinkFileCautious filepath
    checkResponse _ resp bs =
      if responseStatus resp == ok200
      then Nothing
      else Just $ StatusCodeException (fmap (const ()) resp) bs -- :@
    checkExcept :: Either HttpException () -> Maybe HttpException
    checkExcept (Left e) = Just e
    checkExcept _ = Nothing
    exposeFile :: Maybe HttpException -> IO (Either HttpException B.ByteString)
    exposeFile (Just e) = return (Left e)
    exposeFile Nothing = B.readFile filepath >>= \bs -> return (Right bs)
