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


defHost :: Url 'Https
defHost = https "cache.nixos.org"
-- defHost = https "httpbin.org"

main :: IO ()
main =
  print =<<
  downloadFile "nar/1wf20f50458zf9x3dvrhyd04hzn7m71qg24aq1jsv01cvnb692a6.nar.xz"
               "testFile.raw"

downloadFile :: String -> FilePath -> IO (Maybe HttpException)
downloadFile urlpath filepath = checkExcept <$> downloadAndSave
  where
    downloadAndSave = try $ runReq config $
      reqBr GET (defHost /~ urlpath) NoReqBody mempty $ \r ->
      runConduitRes $ responseBodySource r .| sinkFileCautious filepath
    config = def {httpConfigCheckResponse = checkResponse}
    checkResponse _ resp bs =
      if responseStatus resp == ok200
      then Nothing
      else Just $ StatusCodeException (fmap (const ()) resp) bs -- :@
    checkExcept :: Either HttpException () -> Maybe HttpException
    checkExcept (Left e) = Just e
    checkExcept _ = Nothing
