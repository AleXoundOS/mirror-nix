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
import System.Directory (renameFile)
import System.IO (Handle, hClose)
import System.IO.Temp (withTempFile)
import Control.Applicative (liftA2)
import Control.Arrow (left)
import Control.Monad (void)


type UrlEndpoint = Text

data DownloadError = HttpError HttpException
                   | CheckError
  deriving (Show)


defHost :: Url 'Https
defHost = https "cache.nixos.org"
-- defHost = https "httpbin.org"

-- | Downloads a file, checks and writes to a file system. Actually download is
-- a stream of http body to a temporary file; once check is positive, file is
-- renamed to requested name.
downloadCheckAndSave :: UrlEndpoint -> FilePath -> (B.ByteString -> Bool)
                     -> IO (Either DownloadError B.ByteString)
downloadCheckAndSave urlEndPoint path check = withTempFile path template
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
