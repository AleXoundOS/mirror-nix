{-# LANGUAGE FlexibleContexts #-}

module Download.Nix.NarInfos
  ( getNarInfos, GetNarInfosState(..)
  ) where

import Control.Monad.Reader
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Download.Nix.BinaryCache
import Download.Nix.Common
import System.Nix.Derivation hiding (DerivationP(..))
import System.Nix.NarInfo
import System.Nix.StoreNames
import Utils


data GetNarInfosState = GetNarInfosState
  { stNarInfos  :: [NarInfo]
  , stFailed    :: Map StoreName (Maybe DrvPath, String)
  , stHashCache :: HashCache
  , stWantQty   :: Int
  , stCurQty    :: Int
  } deriving (Show)


-- | Recursively downloads and stores all `NarInfo`s comprising the given
-- `StoreName`s. `StoreName`s failed to get immediate `NarInfo` are returned.
getNarInfos :: (MonadReader DownloadAppConfig m, MonadIO m)
  => Map StoreName (Maybe DrvPath) -> m GetNarInfosState
getNarInfos storeNamesMap = do
  putStrLnIO "GET [done/failed/want] store path"
  finalState <- foldM go initialState $ Map.toList storeNamesMap
  printLiveStats finalState
  return finalState
  where
    initialState = GetNarInfosState [] Map.empty mempty (length storeNamesMap) 0
    go :: (MonadReader DownloadAppConfig m, MonadIO m)
       => GetNarInfosState -> (StoreName, Maybe DrvPath) -> m GetNarInfosState
    go state item@(storeName, _) = do
      printLiveStats state >> putStrIO (showStoreNamePath storeName)
      (statusStr, state') <- processResult state item <$>
        recDlStoreNameNarInfos (stNarInfos state, stHashCache state) storeName
      putStrLnIO statusStr
      return state'

processResult
  :: GetNarInfosState
  -> (StoreName, Maybe DrvPath)
  -> Either String ([NarInfo], HashCache)
  -> (String, GetNarInfosState)
processResult state (storeName, mDrvPath) result =
  case result of
    Left errStr ->
      ( " [FAIL]"
      , state'{ stFailed =
                  Map.insert storeName (mDrvPath, errStr) $ stFailed state }
      )
    Right (ns, hs) ->
      ( " [DONE]"
      , state'{ stNarInfos  = ns ++ stNarInfos state
              , stHashCache = hs
              }
      )
  where
    state' = state{stCurQty = stCurQty state + 1}

printLiveStats :: MonadIO m => GetNarInfosState -> m ()
printLiveStats = putStrIO . showStats . getNums
  where
    getNums state = (stCurQty state, length $ stFailed state, stWantQty state)
    showStats (cur, failed, want) =
      "GET [" ++ show cur ++ "/" ++ show failed ++ "/" ++ show want ++ "] "
