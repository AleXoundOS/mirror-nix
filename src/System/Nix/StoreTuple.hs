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
    parseWords3 [] = False
    parseWords3 ["(fixed-output)"] = True
    parseWords3 _ = error "cannot parse kind of output"

writeStoreTuple :: FilePath -> [StoreTuple] -> IO ()
writeStoreTuple fp tuples = T.writeFile fp $ T.unlines $ map textLine tuples
  where
    textLine :: StoreTuple -> Text
    textLine (storeName, mExtra) =
      T.unwords $ justifyStoreName storeName : textExtras mExtra
    textExtras Nothing = []
    textExtras (Just (drvPath, isDrvFixed)) = drvPath : textIsFixed isDrvFixed
    textIsFixed True  = ["(fixed-output)"]
    textIsFixed False = []
    justifyStoreName = T.justifyLeft storePathWidth ' ' . textStoreNamePath
    storePathWidth = maximum $ map (T.length . textStoreNamePath . fst) tuples
