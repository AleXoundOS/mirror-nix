{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Network.HTTP.Req
import qualified Data.ByteString.Lazy as B

import Download


main :: IO ()
main = do
  ss <- readFile "test-data/store-paths"
  print $ allNarsUrlsOfStorePaths $ lines ss
  return ()

allNarsUrlsOfStorePaths :: [FilePath] -> [Url 'Https]
allNarsUrlsOfStorePaths = undefined
  -- where
  --   oneNarRefs :: NarInfoYaml -> 

dlTest :: IO ()
dlTest =
  print =<<
  downloadToFs "nar/1wf20f50458zf9x3dvrhyd04hzn7m71qg24aq1jsv01cvnb692a6.nar.xz"
               "testFile.raw"
