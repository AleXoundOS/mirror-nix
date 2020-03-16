{-# LANGUAGE OverloadedStrings #-}

module System.Nix.StorePath
  ( StorePath, StoreHash
  , mkStoreHashFromStoreName, mkStoreHashFromStorePath
  ) where


import Data.Text (Text)
import qualified Data.Text as T


type StorePath = Text  -- ^ absolute filesystem nix store path
type StoreName = Text  -- ^ part after "\/nix\/store"
type StoreHash = Text  -- ^ base32 hash solely


mkStoreHashFromStorePath :: Text -> Maybe StoreHash
mkStoreHashFromStorePath t =
  mkStoreHashFromStoreName =<< T.stripPrefix "/nix/store/" t

mkStoreHashFromStoreName :: StoreName -> Maybe StoreHash
mkStoreHashFromStoreName t = if all ($ base32hash) [not . T.null, isBase32]
                             then Just base32hash
                             else Nothing
  where
    (base32hash, _rest) = T.splitAt 32 t
    isBase32 = T.all (`elem` ("0123456789abcdfghijklmnpqrsvwxyz" :: String))
