{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Network.HTTP.Req
import qualified Data.ByteString.Lazy as B
import qualified Data.YAML as Y
import Data.Either (isLeft)

import Download
import System.Nix.NarInfo

import qualified Expected as E


main :: IO ()
main = defaultMain narInfoParsing

narInfoParsing :: TestTree
narInfoParsing = testCase "NarInfo parsing" $ do
  bs <- B.readFile "test-data/003jhgnff2dwnz4j23wsqhzx9mdbxrqx.narinfo"
  assertEqual "parse correct NarInfo YAML"
    (Right E.correctNarInfoYAML) (Y.decode1 bs)
  caseParseNarInfo
    "test-data/003jhgnff2dwnz4j23wsqhzx9mdbxrqx-bad1.narinfo"
    "false parse of bad NarInfo YAML"
    isLeft
    (Y.decode1 :: B.ByteString -> Either String NarInfo)
  caseParseNarInfo
    "test-data/003jhgnff2dwnz4j23wsqhzx9mdbxrqx-bad2.narinfo"
    "false parse of bad NarInfo YAML"
    isLeft
    (Y.decode1 :: B.ByteString -> Either String NarInfo)
  caseParseNarInfo
    "test-data/003jhgnff2dwnz4j23wsqhzx9mdbxrqx-bad3.narinfo"
    "false parse of bad NarInfo YAML"
    isLeft
    (Y.decode1 :: B.ByteString -> Either String NarInfo)
  caseParseNarInfo
    "test-data/003jhgnff2dwnz4j23wsqhzx9mdbxrqx-bad4.narinfo"
    "false parse of bad NarInfo YAML"
    isLeft
    (Y.decode1 :: B.ByteString -> Either String NarInfo)

-- | Make a test case which reads a file, applies a function to it and asserts
-- the result with the given condition.
caseParseNarInfo :: FilePath -> String -> (b -> Bool) -> (B.ByteString -> b)
                 -> Assertion
caseParseNarInfo filepath msg cond func =
  (cond . func <$> B.readFile filepath) @? msg
