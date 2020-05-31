{-# LANGUAGE OverloadedStrings #-}

module System.Nix.StoreTuple
  ( StoreTuple, StoreExtra
  , readStoreTuple, writeStoreTuple
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T (readFile, writeFile)

import System.Nix.Derivation
import System.Nix.StoreNames

import Utils (forceEitherStr)


type StoreExtra = (DrvPath, DrvIsFixed)
type StoreTuple = (StoreName, Maybe StoreExtra)


readStoreTuple :: FilePath -> IO [StoreTuple]
readStoreTuple = fmap (map (parseWords . T.words) . T.lines) . T.readFile
  where
    parseWords :: [Text] -> StoreTuple
    parseWords [] = error "empty line"
    parseWords (storePath:words23) =
      ( forceEitherStr $ stripParseStoreName storePath
      , parseWords23 words23
      )
    parseWords23 [] = Nothing
    parseWords23 (drvPath:words3) = Just (drvPath, parseWords3 words3)
    parseWords3 ["(fixed-output)"]     = DrvIsFixed
    parseWords3 ["(non-fixed-output)"] = DrvIsNotFixed
    parseWords3 []                     = DrvFixedUnknown
    parseWords3 _ = error "cannot parse kind of output"

writeStoreTuple :: FilePath -> [StoreTuple] -> IO ()
writeStoreTuple fp tuples = T.writeFile fp $ T.unlines $ map textLine tuples
  where
    textLine :: StoreTuple -> Text
    textLine (storeName, mExtra) =
      T.unwords $ justifyStoreName storeName : textExtras mExtra
    textExtras Nothing = []
    textExtras (Just (drvPath, isDrvFixed)) = drvPath : textIsFixed isDrvFixed
    textIsFixed DrvIsFixed      = ["(fixed-output)"]
    textIsFixed DrvIsNotFixed   = ["(non-fixed-output)"]
    textIsFixed DrvFixedUnknown = []
    justifyStoreName = T.justifyLeft storePathWidth ' ' . textStoreNamePath
    storePathWidth = maximum $ map (T.length . textStoreNamePath . fst) tuples
