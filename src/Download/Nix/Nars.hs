{-# LANGUAGE FlexibleContexts #-}

module Download.Nix.Nars
  ( dlNars, DownloadNarsState(..)
  , downloadNar
  ) where

import Control.Monad.Reader
import Network.HTTP.Req

import Download.Nix.Common
import System.Nix.NarInfo
import System.Nix.StoreNames
import Utils
import qualified Data.Text as T
import qualified System.Nix.Base32 as NixBase32


data DownloadNarsState = DownloadNarsState
  { stStored    :: [FilePath]
  , stFailed    :: [(NarInfo, String)]
  , stWantQty   :: Int
  , stFailedQty :: Int
  , stCurQty    :: Int
  } deriving (Show)


dlNars :: (MonadReader DownloadAppConfig m, MonadIO m)
  => [NarInfo] -> m DownloadNarsState
dlNars ns = do
  putStrLnIO "NAR GET [done/failed/want] store path"
  finalState <- foldM go initialState ns
  printLiveStats finalState >> putStrIO "\n"
  return finalState
  where
    initialState = DownloadNarsState [] [] (length ns) 0 0
    go :: (MonadReader DownloadAppConfig m, MonadIO m)
       => DownloadNarsState -> NarInfo -> m DownloadNarsState
    go state narinfo = do
      printLiveStats state
        >> putStrIO (showStoreNamePath $ _storeName narinfo)
      (statusStr, state') <-
        processResult state narinfo <$> downloadNar narinfo
      putStrLnIO statusStr
      return state'

-- | Download a file in a streaming manner, validating its checksum before
-- renaming from temporary.
downloadNar :: (MonadReader DownloadAppConfig m, MonadIO m)
            => NarInfo -> m (Either DownloadError FilePath)
downloadNar ni = do
  bcBaseUrl <- mkRequest <$> asks appBcBaseUrl
  downloadCheckAndSave
    (mapBoth mkUrl mkUrl bcBaseUrl) (T.unpack endp) (sinkSha256, hash)
  where
    mkUrl (base, opt) = (base /: endp, opt)
    endp = _url ni
    hash = NixBase32.decode $ _fileHash ni

mapBoth :: (a -> c) -> (b -> d) -> Either a b -> Either c d
mapBoth f _ (Left x)  = Left (f x)
mapBoth _ f (Right x) = Right (f x)

processResult :: DownloadNarsState -> NarInfo -> Either DownloadError FilePath
              -> (String, DownloadNarsState)
processResult state narinfo result =
  case result of
    Left de ->
      ( " [FAIL]"
      , state'{ stFailed    = (narinfo, showDownloadError de) : stFailed state
              , stFailedQty = stFailedQty state + 1
              }
      )
    Right fp ->
      ( " [DONE]"
      , state'{ stStored = fp : stStored state }
      )
  where
    state' = state{stCurQty = stCurQty state + 1}

printLiveStats :: MonadIO m => DownloadNarsState -> m ()
printLiveStats = putStrIO . showStats . getNums
  where
    getNums state = (stCurQty state, length $ stFailed state, stWantQty state)
    showStats (cur, failed, want) =
      "GET [" ++ show cur ++ "/" ++ show failed ++ "/" ++ show want ++ "] "
