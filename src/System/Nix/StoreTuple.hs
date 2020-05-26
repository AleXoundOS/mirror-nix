module System.Nix.StoreTuple
  ( StoreTuple
  , readStoreTuple, writeStoreTuple
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T (readFile, writeFile)
import Data.Maybe (fromJust)

import System.Nix.Derivation
import System.Nix.StoreNames

import Utils (forceEitherStr)


type StoreTuple = (StoreName, Maybe DrvPath)


readStoreTuple :: FilePath -> IO [StoreTuple]
readStoreTuple = (map parseLine . T.lines <$>) . T.readFile
  where
    parseLine :: Text -> (StoreName, Maybe DrvPath)
    parseLine line
      | (storePathTxt:ws) <- T.words line =
          ( parseStoreName' storePathTxt, parseDrvPath ws)
      | otherwise = error "empty line"
    parseStoreName' = forceEitherStr . stripParseStoreName
    parseDrvPath [] = Nothing
    parseDrvPath [drvPath] = Just drvPath
    parseDrvPath _ = error "extra words after derivation path"

writeStoreTuple :: FilePath -> [StoreTuple] -> IO ()
writeStoreTuple fp tuples = T.writeFile fp $ T.unlines $ map showLine tuples
  where
    showLine :: StoreTuple -> Text
    showLine (storeName, mDrvPath) = fromJust
      $ (Just $ T.justifyLeft storePathWidth ' ' $ textStoreNamePath storeName)
      <> mDrvPath
    storePathWidth = (+1)
      $ maximum $ map (T.length . textStoreNamePath . fst) tuples
