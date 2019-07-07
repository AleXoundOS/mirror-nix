{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Yaml as Y
  (decodeFileEither, decodeEither', prettyPrintParseException)

import System.Nix.NarInfo

import qualified Expected as E


main :: IO ()
main = defaultMain narInfoParsing

narInfoParsing :: TestTree
narInfoParsing = testCase "NarInfo parsing" $ do
  isEqualParseNarInfo "parse correct NarInfo YAML"
    "test-data/003jhgnff2dwnz4j23wsqhzx9mdbxrqx.narinfo"
    E.rightNarInfoYaml1
  isEqualParseNarInfo "parse correct NarInfo YAML with empty References"
    "test-data/003jhgnff2dwnz4j23wsqhzx9mdbxrqx-empty-references.narinfo"
    E.rightNarInfoYaml2
  isEqualParseNarInfo "parse correct NarInfo YAML with empty References"
    "test-data/003jhgnff2dwnz4j23wsqhzx9mdbxrqx-no-deriver.narinfo"
    E.rightNarInfoYaml3
  isEqualParseNarInfo "parse of bad NarInfo YAML 1"
    "test-data/003jhgnff2dwnz4j23wsqhzx9mdbxrqx-bad1.narinfo"
    E.leftNarInfo1
  isEqualParseNarInfo "parse of bad NarInfo YAML 2"
    "test-data/003jhgnff2dwnz4j23wsqhzx9mdbxrqx-bad2.narinfo"
    E.leftNarInfo2
  isEqualParseNarInfo "parse of bad NarInfo YAML 3"
    "test-data/003jhgnff2dwnz4j23wsqhzx9mdbxrqx-bad3.narinfo"
    E.leftNarInfo3

-- | Make a test case which reads a file, applies a function to it and asserts
-- the result with the given condition.
isEqualParseNarInfo
  :: HasCallStack => String -> FilePath -> Either String NarInfo -> Assertion
isEqualParseNarInfo msg filepath expected =
  assertEqual msg expected
  =<< (either (Left . Y.prettyPrintParseException) Right
        <$> Y.decodeFileEither filepath)
