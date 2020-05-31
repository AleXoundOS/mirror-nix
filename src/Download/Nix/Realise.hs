{-# LANGUAGE FlexibleContexts #-}

module Download.Nix.Realise
  ( realiseAndCopyPaths, realiseAndCopyPath, RealiseCopyPathsState(..)
  ) where

import Control.Monad.Reader
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Download.Nix.Common
import System.Nix.NixToolsProc
import System.Nix.StoreNames
import System.Nix.StoreTuple
import Utils


data RealiseCopyPathsState = RealiseCopyPathsState
  { stStatuses  :: [(StoreTuple, RealiseCopyStatus)]
  , stWantQty   :: Int
  , stFailedQty :: Int
  , stCurQty    :: Int
  } deriving (Show)

type RealiseCopyStatus = Either RealiseError [StoreName]

data RealiseError = RealiseErrorExitFailure (ExitCode, ByteString)
                  | RealiseErrorMismatch [StoreName]
                  | RealiseErrorNoDerivation
  deriving (Show)


realiseAndCopyPaths :: (MonadReader DownloadAppConfig m, MonadIO m)
  => String -> Map StoreName (Maybe StoreExtra) -> m RealiseCopyPathsState
realiseAndCopyPaths signKey targetStoreMap = do
  putStrLnIO "REALISE [done/failed/want] store path"
  finalState <- foldM go initialState $ Map.toList targetStoreMap
  printLiveStats finalState >> putStrIO "\n"
  return finalState
  where
    initialState = RealiseCopyPathsState [] (length targetStoreMap) 0 0
    go :: (MonadReader DownloadAppConfig m, MonadIO m)
       => RealiseCopyPathsState -> StoreTuple -> m RealiseCopyPathsState
    go state storeTuple@(storeName, _) = do
      printLiveStats state >> putStrIO (showStoreNamePath storeName)
      (statusStr, state') <-
        processResult state storeTuple <$> realiseAndCopyPath signKey storeTuple
      putStrLnIO statusStr
      return state'

-- | Realise derivation and `nix copy` output store paths.
realiseAndCopyPath :: (MonadReader DownloadAppConfig m, MonadIO m)
  => String -> StoreTuple -> m RealiseCopyStatus
realiseAndCopyPath signKey (storeName, Just (drvPath, _)) = do
  realiseRes <- liftIO (nixStoreRealiseDrv drvPath)
  case realiseRes of
    Left err -> return $ Left $ RealiseErrorExitFailure err
    Right realisedNames ->
      if storeName `elem` realisedNames
      then do
        copy $ map showStoreNamePath realisedNames
        return $ Right realisedNames
      else return $ Left $ RealiseErrorMismatch realisedNames
  where
    copy storePaths = do
      liftIO $ nixSignPaths storePaths signKey
      basePath <- asks appCachePath
      liftIO $ nixCopyPaths storePaths basePath
realiseAndCopyPath _ (_, Nothing) = return $ Left RealiseErrorNoDerivation

-- textRealiseError :: RealiseError -> Text
-- textRealiseError = undefined
-- "realised " ++ show storeNames ++ "\nwhile wanted " ++
--            showStoreNamePath storeName ++ " is missing!"

processResult
  :: RealiseCopyPathsState -> StoreTuple -> RealiseCopyStatus
  -> (String, RealiseCopyPathsState)
processResult state tg result =
  case result of
    Left _ ->
      ( " [FAIL]"
      , state'{ stFailedQty = stFailedQty state + 1 }
      )
    Right _ ->
      ( " [DONE]"
      , state'
      )
  where state' = state{ stStatuses = (tg, result) : stStatuses state
                      , stCurQty = stCurQty state + 1
                      }

printLiveStats :: MonadIO m => RealiseCopyPathsState -> m ()
printLiveStats = putStrIO . showStats . getNums
  where
    getNums state = (stCurQty state, stFailedQty state, stWantQty state)
    showStats (cur, failed, want) =
      "REALISE [" ++ show cur ++ "/" ++ show failed ++ "/" ++ show want ++ "] "
