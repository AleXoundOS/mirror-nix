{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module System.Nix.StoreNames
  ( StoreName(..), StoreHash, DrvName, StorePath, OutName, StoreOut
  , stripNixStorePrefix, mkStoreHash, parseStoreName
  , storeNameHash
  , stripParseStoreName
  , textStoreNamePath, textStoreName
  , showStoreNamePath
  ) where


import Control.Monad ((<=<))
import qualified Data.Set as Set (fromList, member)
import Data.Text (Text)
import Prelude hiding (fail)
import qualified Data.Text as T

-- | comprises absolute filesystem nix store path
newtype StoreName = StoreName { unStoreName :: (StoreHash, DrvName) }
  deriving (Ord, Eq, Show)

type StoreHash = Text  -- ^ base32 hash solely
type DrvName   = Text  -- ^ part after hash

type StorePath = Text

-- type StoreName = Text  -- ^ part after "\/nix\/store"
type OutName = Text
type StoreOut = (OutName, StoreName)


stripNixStorePrefix :: Text -> Either String Text
stripNixStorePrefix =
  maybe (Left "strip prefix") Right . T.stripPrefix "/nix/store/"

mkStoreHash :: Text -> Either String StoreHash
mkStoreHash t =
  fmap (fst . unStoreName) . parseStoreName =<< stripNixStorePrefix t

parseStoreName :: Text -> Either String StoreName
parseStoreName t = if    all ($ base32hash) [not . T.null, isBase32]
                      && all ($ drvName)    [not . T.null, isNotForbidden]
                   then Right $ StoreName (base32hash, drvName)
                   else Left $ "cannot parse StoreName: " ++ T.unpack t
  where
    (base32hash, drvName') = T.splitAt 32 t
    drvName = T.drop 1 drvName'
    isBase32 = T.all (`Set.member` validHashChars)
    isNotForbidden = T.all (not . flip elem ("\\/()" :: String))
    validHashChars = Set.fromList "0123456789abcdfghijklmnpqrsvwxyz"

stripParseStoreName :: Text -> Either String StoreName
stripParseStoreName = parseStoreName <=< stripNixStorePrefix

textStoreName :: StoreName -> Text
textStoreName (StoreName (hash, drvName)) = hash <> "-" <> drvName

textStoreNamePath :: StoreName -> Text
textStoreNamePath name = "/nix/store/" <> textStoreName name

showStoreNamePath :: StoreName -> String
showStoreNamePath = T.unpack . textStoreNamePath

storeNameHash :: StoreName -> StoreHash
storeNameHash = fst . unStoreName
