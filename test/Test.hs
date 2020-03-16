{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Yaml as Y (decodeFileEither)
import qualified Data.ByteString as B (readFile)

import System.Nix.Derivation
import System.Nix.NarInfo

import qualified Expected as E


main :: IO ()
main = defaultMain $ testGroup "tests" [narInfoParsing, jsonDerivationParse]

narInfoParsing :: TestTree
narInfoParsing = testCase "NarInfo parsing" $ do
  isEqualParseNarInfo "parse correct NarInfo YAML"
    "test-data/003jhgnff2dwnz4j23wsqhzx9mdbxrqx.narinfo"
    E.justNarInfoYaml1
  isEqualParseNarInfo "parse correct NarInfo YAML with empty References"
    "test-data/003jhgnff2dwnz4j23wsqhzx9mdbxrqx-empty-references.narinfo"
    E.justNarInfoYaml2
  isEqualParseNarInfo "parse correct NarInfo YAML with empty References"
    "test-data/003jhgnff2dwnz4j23wsqhzx9mdbxrqx-no-deriver.narinfo"
    E.justNarInfoYaml3
  isEqualParseNarInfo "parse of bad NarInfo YAML 1"
    "test-data/003jhgnff2dwnz4j23wsqhzx9mdbxrqx-bad1.narinfo"
    E.noNarInfo1
  isEqualParseNarInfo "parse of bad NarInfo YAML 2"
    "test-data/003jhgnff2dwnz4j23wsqhzx9mdbxrqx-bad2.narinfo"
    E.noNarInfo2
  isEqualParseNarInfo "parse of bad NarInfo YAML 3"
    "test-data/003jhgnff2dwnz4j23wsqhzx9mdbxrqx-bad3.narinfo"
    E.noNarInfo3

-- | Make a test case which reads a file, applies a function to it and asserts
-- the result with the given condition.
isEqualParseNarInfo
  :: HasCallStack => String -> FilePath -> Maybe NarInfo -> Assertion
isEqualParseNarInfo msg filepath expected =
  assertEqual msg expected =<< rightToMaybe <$> Y.decodeFileEither filepath

jsonDerivationParse :: TestTree
jsonDerivationParse = testGroup "JSON derivation parsing"
  [ testCase "good JSON derivation 1"
    $ assertEqual "parse single JSON derivation" E.jsonDerivation1
    . rightToMaybe . eitherDecodeStrict'
    =<< B.readFile "test-data/show-derivation-1.json"
  , testCase "bad JSON derivation 1 (bad prefix)"
    $ (E.noDerivation1 @=?)
    . rightToMaybe . eitherDecodeStrict'
    =<< B.readFile "test-data/show-derivation-1-bad-prefix.json"
  ]

rightToMaybe :: Either l r -> Maybe r
rightToMaybe = either (const Nothing) Just
