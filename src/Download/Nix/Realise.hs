{-# LANGUAGE FlexibleContexts #-}

module Download.Nix.Realise
  ( realiseAndCopyPaths, realiseAndCopyPath, RealiseCopyPathsState(..)
  ) where

import Control.Monad.Reader
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Download.Nix.Common
import System.Nix.Derivation hiding (DerivationP(..))
import System.Nix.NixToolsProc
import System.Nix.StoreNames
import Utils


data RealiseCopyPathsState = RealiseCopyPathsState
  { stStatuses  :: [(RealiseCopyPath, RealiseCopyStatus)]
  , stWantQty   :: Int
  , stFailedQty :: Int
  , stCurQty    :: Int
  } deriving (Show)

type RealiseCopyPath = (StoreName, (Maybe DrvPath, String))
type RealiseCopyStatus = Either RealiseError [StoreName]

data RealiseError = RealiseErrorExitFailure (ExitCode, ByteString)
                  | RealiseErrorMismatch [StoreName]
                  | RealiseErrorNoDerivation
  deriving (Show)


realiseAndCopyPaths :: (MonadReader DownloadAppConfig m, MonadIO m)
                    => String -> Map StoreName (Maybe DrvPath, String)
                    -> m RealiseCopyPathsState
realiseAndCopyPaths signKey inpMap = do
  putStrLnIO "GET [done/failed/want] store path"
  finalState <- foldM go initialState $ Map.toList inpMap
  printLiveStats finalState >> putStrIO "\n"
  return finalState
  where
    initialState = RealiseCopyPathsState [] (length inpMap) 0 0
    go :: (MonadReader DownloadAppConfig m, MonadIO m)
       => RealiseCopyPathsState -> RealiseCopyPath -> m RealiseCopyPathsState
    go state inp@(storeName, _) = do
      printLiveStats state >> putStrIO (showStoreNamePath storeName)
      (statusStr, state') <-
        processResult state inp <$> realiseAndCopyPath signKey inp
      putStrLnIO statusStr
      return state'

-- | Realise derivation and `nix copy` output store paths.
realiseAndCopyPath :: (MonadReader DownloadAppConfig m, MonadIO m)
  => String -> RealiseCopyPath -> m RealiseCopyStatus
realiseAndCopyPath signKey (storeName, (Just drvPath, _)) = do
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
realiseAndCopyPath _ (_, (Nothing, _)) = return $ Left RealiseErrorNoDerivation

-- textRealiseError :: RealiseError -> Text
-- textRealiseError = undefined
-- "realised " ++ show storeNames ++ "\nwhile wanted " ++
--            showStoreNamePath storeName ++ " is missing!"

processResult
  :: RealiseCopyPathsState -> RealiseCopyPath -> RealiseCopyStatus
  -> (String, RealiseCopyPathsState)
processResult state inp result =
  case result of
    Left _ ->
      ( " [FAIL]"
      , state'{ stFailedQty = stFailedQty state + 1 }
      )
    Right _ ->
      ( " [DONE]"
      , state'
      )
  where state' = state{ stStatuses = (inp, result) : stStatuses state
                      , stCurQty = stCurQty state + 1
                      }

printLiveStats :: MonadIO m => RealiseCopyPathsState -> m ()
printLiveStats = putStrIO . showStats . getNums
  where
    getNums state = (stCurQty state, stFailedQty state, stWantQty state)
    showStats (cur, failed, want) =
      "REALISE [" ++ show cur ++ "/" ++ show failed ++ "/" ++ show want ++ "] "
