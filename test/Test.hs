{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Network.HTTP.Req
import Network.HTTP.Req.Conduit
import qualified Data.ByteString.Char8 as BS
  (ByteString, putStrLn, writeFile)
import Data.Default.Class
import Control.Monad.IO.Class
import Conduit ((.|), runConduitRes, sinkFileCautious)


testHostname :: Url 'Https
testHostname = https "cache.nixos.org"

main :: IO ()
main =
  downloadFile "nar/1wf20f50458zf9x3dvrhyd04hzn7m71qg24aq1jsv01cvnb692a6.nar.xz"
  -- BS.writeFile "testData.raw"
  -- =<< downloadFile
  -- "nar/1wf20f50458zf9x3dvrhyd04hzn7m71qg24aq1jsv01cvnb692a6.nar.xz"

downloadFile :: String -> IO ()
downloadFile path = runReq def $ do
  reqBr GET (testHostname /~ path) NoReqBody mempty $ \r ->
    runConduitRes $ responseBodySource r .| sinkFileCautious "testFile.raw"
