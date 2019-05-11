{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Network.HTTP.Req
import Network.HTTP.Req.Conduit (responseBodySource)
import Network.HTTP.Client (responseStatus, HttpExceptionContent(..))
import Network.HTTP.Types.Status (ok200)
import Data.Default.Class (def)
import Conduit ((.|), runConduitRes, sinkFileCautious)
import Control.Exception (try)

import qualified Data.ByteString.Lazy as B


defHost :: Url 'Https
defHost = https "cache.nixos.org"
-- defHost = https "httpbin.org"

main :: IO ()
main =
  print =<<
  downloadToFs "nar/1wf20f50458zf9x3dvrhyd04hzn7m71qg24aq1jsv01cvnb692a6.nar.xz"
               "testFile.raw"

downloadToFs :: String -> FilePath -> IO (Either HttpException B.ByteString)
downloadToFs urlpath filepath = exposeFile =<< checkExcept <$> tryStreamDownload
  where
    config = def {httpConfigCheckResponse = checkResponse}
    tryStreamDownload = try $ runReq config
      $ reqBr GET (defHost /~ urlpath) NoReqBody mempty $ \r ->
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
