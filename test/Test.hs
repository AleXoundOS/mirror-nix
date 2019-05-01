{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Main
  ( main, downloadFile
  ) where

import Network.HTTP.Req
import qualified Data.ByteString.Lazy.Char8 as LBS
  (ByteString, putStrLn, writeFile)
-- import qualified Data.Text as T (Text)
import Data.Default.Class
import Control.Monad.IO.Class


testUrl :: Url 'Https
testUrl = https "cache.nixos.org" /: "37a8nkijf2vy350q3lxhjmxwdnpm6lnf.narinfo"

main :: IO ()
main = downloadFile

downloadFile :: IO ()
downloadFile = runReq def $ do
  lbs <- req GET testUrl NoReqBody lbsResponse mempty
  liftIO $ LBS.putStrLn (responseBody lbs)
