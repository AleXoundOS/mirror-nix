{-# LANGUAGE FlexibleContexts #-}

module Download.Nix.NarInfos
  ( getNarInfos, GetNarInfosState(..), NarInfoDlErr
  ) where

import Control.Monad.Reader
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Download.Nix.BinaryCache
import Download.Nix.Common
import System.Nix.NarInfo
import System.Nix.StoreNames
import System.Nix.StoreTuple
import Utils

newtype NarInfoDlErr = NarInfoDlErr String
  deriving (Eq, Show)

data GetNarInfosState = GetNarInfosState
  { stNarInfos  :: [NarInfo]
  , stFailed    :: Map StoreName (Maybe StoreExtra, NarInfoDlErr)
  , stHashCache :: HashCache
  , stWantQty   :: Int
  , stCurQty    :: Int
  } deriving (Eq, Show)


-- | Recursively downloads and stores all `NarInfo`s comprising the given
-- `StoreName`s. `StoreName`s failed to get immediate `NarInfo` are returned.
getNarInfos :: (MonadReader DownloadAppConfig m, MonadIO m)
  => Map StoreName (Maybe StoreExtra) -> m GetNarInfosState
getNarInfos storeNamesMap = do
  putStrLnIO "GET [(narinfos) done/failed/want] store path"
  finalState <- foldM go initialState $ Map.toList storeNamesMap
  printLiveStats finalState >> putStrIO "\n"
  if length (stNarInfos finalState) == length (stHashCache finalState)
    then return finalState
    else error
         "length (stNarInfos finalState) == length (stHashCache finalState)"
  where
    initialState = GetNarInfosState [] Map.empty mempty (length storeNamesMap) 0
    go :: (MonadReader DownloadAppConfig m, MonadIO m)
      => GetNarInfosState -> (StoreName, Maybe StoreExtra)
      -> m GetNarInfosState
    go state item@(storeName, _) = do
      printLiveStats state >> putStrIO (showStoreNamePath storeName)
      (statusStr, state') <- processResult state item <$>
        recDlStoreNameNarInfos (stNarInfos state, stHashCache state) storeName
      putStrLnIO statusStr
      return state'

processResult
  :: GetNarInfosState
  -> (StoreName, Maybe StoreExtra)
  -> Either String ([NarInfo], HashCache)
  -> (String, GetNarInfosState)
processResult state (storeName, mExtra) result =
  case result of
    Left errStr ->
      ( " [FAIL]"
      , state'{ stFailed = mkStFailed errStr }
      )
    Right (ns, hs) ->
      ( " [DONE]"
      , state'{ stNarInfos  = ns
              , stHashCache = hs
              }
      )
  where
    state' = state{stCurQty = stCurQty state + 1}
    mkStFailed errStr =
      Map.insert storeName (mExtra, NarInfoDlErr errStr) $ stFailed state

printLiveStats :: MonadIO m => GetNarInfosState -> m ()
printLiveStats = putStrIO . showStats . getNums
  where
    getNums state =
      ( length $ stHashCache state
      , stCurQty state, length $ stFailed state, stWantQty state )
    showStats (hashCacheLen, cur, failed, want) =
      "GET [(" ++ show hashCacheLen ++ ") "
      ++ show cur ++ "/" ++ show failed ++ "/" ++ show want ++ "] "
