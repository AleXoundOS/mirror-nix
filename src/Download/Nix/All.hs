{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

module Download.Nix.All
  ( StorePathsSourcesInput(..), StorePathsSources(..)
  , GetPathStatus(..)
  , getStorePathsSources, getAllPaths, getStorePathsCache
  , printSourcesStats
  , instantiateMissingEnvDrvs
  ) where

import Control.Monad.Reader
import Data.Containers.ListUtils (nubOrd)
import Data.HashMap.Strict (HashMap)
import Data.List (foldl')
import Data.Map.Strict (Map)
import System.Directory (doesFileExist)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Download.Nix.BinaryCache
import Download.Nix.Common
import Download.Nix.Realise
import System.Nix.Derivation (DerivationP) -- lol ?
import System.Nix.Derivation hiding (DerivationP(..))
import System.Nix.EnvDrvInfo as E
import System.Nix.FixedOutput as F
import System.Nix.NixToolsProc
import System.Nix.StoreNames
import Utils (forceEitherStr, putStrIO, putStrLnIO)


data StorePathsSourcesInput = StorePathsSourcesInput
  -- | store-paths.xz
  { srcInputChannel              :: Maybe FilePath
  -- | nix-instantiate nixos/release-combined.nix
  , srcInputNixosReleaseCombined :: Maybe FilePath
  -- | nix-env -qaP .\/outpaths.nix (from ofborg) @ pkgs\/top-level\/release.nix
  , srcInputNixpkgsRelease       :: Maybe FilePath
  -- | maintainers\/scripts\/all-tarballs.nix
  , srcInputNixpkgsReleaseFixed  :: Maybe FilePath
  -- | nixpkgs string
  , srcInputNixpkgs              :: String    -- ^ -I nixpkgs=`srcFilesNixpkgs`
  , srcInputSystems              :: [String]  -- ^ i.e. ["x86_64-linux"]
  }
  deriving (Show)

{- | Collection of typical sources of Nix/OS store paths and derivations which
constitute everything (I hope) you need for offline workflow.
-}
data StorePathsSources = StorePathsSources
  -- | store-paths.xz
  { sourceChannel              :: ![StorePath]
  -- | nix-instantiate nixos/release-combined.nix
  , sourceNixosReleaseCombined :: !(HashMap DrvPath DerivationP)
  -- | nix-env -qaP .\/outpaths.nix (from ofborg) @ pkgs\/top-level\/release.nix
  , sourceNixpkgsRelease       :: ![EnvDrvInfo]
  -- | maintainers\/scripts\/all-tarballs.nix
  , sourceNixpkgsReleaseFixed  :: ![FixedOutputInfo]
  }
  deriving (Show)

data GetPathStatus = DownloadedFromServer | BuiltLocally | StatusFailed
  deriving (Eq, Show)


-- | Get collection of (hopefully) all possible store paths and derivations by
-- the means of calling specific nix tools and scripts.
getStorePathsSources :: StorePathsSourcesInput -> IO StorePathsSources
getStorePathsSources (StorePathsSourcesInput
                       fpChannel
                       fpNixosReleaseCombined
                       fpNixpkgsRelease
                       fpNixpkgsReleaseFixed
                       nixpkgs systemsList) =
  StorePathsSources
    <$> maybe' (fmap T.lines . T.readFile)              fpChannel
    <*> maybe' (nixShowDerivationsARec nixpkgs args []) fpNixosReleaseCombined
    <*> maybe' (nixEnvQueryAvail nixpkgs args . (:[]))  fpNixpkgsRelease
    <*> maybe' (instantiateFixedOutputs nixpkgs args)   fpNixpkgsReleaseFixed
  where
    maybe' f (Just a) = f a
    maybe' _ Nothing = return mempty
    args = [("supportedSystems", unNixList $ mkNixStrList systemsList)]

-- | Instantiate derivations, missing in /nix/store, but obtained by nix-env
-- (side effect!).
instantiateMissingEnvDrvs :: Nixpkgs -> [String] -> [FilePath] -> [EnvDrvInfo]
                          -> IO [DrvPath]
instantiateMissingEnvDrvs nixpkgs systemsList files envDrvInfos = do
  missingDrvs <- map (T.unpack . _attrPath) <$>
    filterM (fmap not . doesFileExist . T.unpack . _drvPath) envDrvInfos
  -- (side effect in /nix/store)
  batchInstantiateInfos missingDrvs
  where
    batchInstantiateInfos = nixInstantiateAttrsB 100 nixpkgs args files
    args = [("supportedSystems", unNixList $ mkNixStrList systemsList)]

{- | Given 4 sources, get all \/nix\/store\/ paths with corresponding derivation
paths (if possible) with the help of nix cli tools.
-}
getAllPaths :: StorePathsSources -> IO (Map StoreName (Maybe DrvPath))
getAllPaths (StorePathsSources
              srcChannel
              srcNixosReleaseCombined
              srcNixpkgsRelease
              srcNixpkgsReleaseFixed) =
  -- paths, those we get directly (purely) from source data
  let pathsDirect = Map.unions
        [ channelPathsMap
        , nixpkgsReleasePathsMap
        , nixpkgsReleaseFixedPathsMap
        ]

      channelPathsMap = Map.fromList
        $ map ((, Nothing) . forceEitherStr . stripParseStoreName) srcChannel

      nixpkgsReleasePathsMap = Map.fromList
        $ concatMap envDrvInfoPaths srcNixpkgsRelease

      nixpkgsReleaseFixedPathsMap = Map.fromList
        $ map (\foInfo -> ( forceEitherStr $ parseStoreName $ _path foInfo
                          , Just $ _drv foInfo )
              ) srcNixpkgsReleaseFixed

      drvPaths = nubOrd
        (  map E._drvPath srcNixpkgsRelease
        ++ map F._drv     srcNixpkgsReleaseFixed
        )
  in do
    -- discover more store paths recursively from derivation paths
    pathsDiscovered <- drvMapToStoreMap
                       <$> nixShowDerivationsRecB 10000 drvPaths
    -- discovered paths (always have DrvPath) take precedence over direct
    return $ Map.unions
      [drvMapToStoreMap srcNixosReleaseCombined, pathsDiscovered, pathsDirect]

-- | All paths from `EnvDrvInfo` with `StoreName`<->`DrvPath` assoc normalized.
envDrvInfoPaths :: EnvDrvInfo -> [(StoreName, Maybe DrvPath)]
envDrvInfoPaths envDrvInfo = map (, Just $ _drvPath envDrvInfo) outputs
  where
    outputs :: [StoreName]
    outputs = map (forceEitherStr . parseStoreName . snd) $ _outputs envDrvInfo

-- | Transpose [StoreName] from every DerivationP to keys.
drvMapToStoreMap :: HashMap DrvPath DerivationP
                 -> Map StoreName (Maybe DrvPath)
drvMapToStoreMap = HM.foldlWithKey' addPathsFromDerivation Map.empty
  where
    -- insert [StoreName] of a single derivation as keys
    addPathsFromDerivation
      :: Map StoreName (Maybe DrvPath) -> DrvPath -> DerivationP
      -> Map StoreName (Maybe DrvPath)
    addPathsFromDerivation storeMap drvPath =
      foldl' (addPathFromDerivation drvPath) storeMap . allDerivationPaths
    -- insert StoreName as key (ignoring duplicate keys)
    addPathFromDerivation
      :: DrvPath -> Map StoreName (Maybe DrvPath) -> StoreName
      -> Map StoreName (Maybe DrvPath)
    addPathFromDerivation drvPath storeMap storeName =
      Map.insertWith keepOldValue storeName (Just drvPath) storeMap
    keepOldValue = flip const

-- | Downloads binary cache for given `StoreName`s. In case of failure
-- tries to build the corresponding derivations locally, for example, unfree.
getStorePathsCache :: (MonadReader DownloadAppConfig m, MonadIO m)
  => String -> Map StoreName (Maybe DrvPath)
  -> m [(GetPathStatus, (StoreName, Maybe DrvPath))]
getStorePathsCache signKey = fmap fst . foldM add ([], mempty) . Map.toList
  where
    add :: (MonadReader DownloadAppConfig m, MonadIO m)
        => ([(GetPathStatus, (StoreName, Maybe DrvPath))], HashCache)
        -> (StoreName, Maybe DrvPath)
        -> m ([(GetPathStatus, (StoreName, Maybe DrvPath))], HashCache)
    add (resAcc, hs) x =
      (\(res, hs') -> (res : resAcc, hs')) <$> getPath hs x signKey

-- | Downloads binary cache for a single `StoreName`. In case of failure
-- tries to build the corresponding derivation locally, for example, unfree.
getPath :: (MonadReader DownloadAppConfig m, MonadIO m)
  => HashCache -> (StoreName, Maybe DrvPath) -> String
  -> m ((GetPathStatus, (StoreName, Maybe DrvPath)), HashCache)
getPath hs (storeName, mDrvPath) signKey = do
  (result, hs') <- go
  return ((result, (storeName, mDrvPath)), hs')
    where
      go = do
        putStrIO $ "[GET] " ++ showStoreNamePath storeName
        mHashCache <- undefined hs storeName
        case mHashCache of
          Just hs' -> do
            putStrLnIO " [DONE]"
            return (DownloadedFromServer, hs')
          Nothing -> do
            putStrLnIO " [FAIL]"
            case mDrvPath of
              Just drvPath -> do
                putStrIO $ "[REALISE] " ++ T.unpack drvPath
                realisedStoreNames <-
                  realiseAndCopyPath signKey (storeName, (mDrvPath, undefined))
                case realisedStoreNames of
                  Left errStr -> do
                    putStrLnIO "[FAIL]"
                    putStrLnIO errStr
                    return (StatusFailed, hs)
                  Right storeNames -> do
                    putStrLnIO ":\n"
                    liftIO $ mapM_ printRealisedStorePath storeNames
                    return (BuiltLocally, hs)
              Nothing -> do
                putStrLnIO "[FAIL]"
                putStrLnIO "- ^ no derivation path available!"
                return (StatusFailed, hs)
      printRealisedStorePath =
        putStrLn . flip (++) " [DONE]" . (++) "-> " . showStoreNamePath

-- | TODO Analyze store paths income from every source.
-- analyzeStorePathsIncome ::

printSourcesStats :: StorePathsSources -> IO ()
printSourcesStats
  (StorePathsSources
   storePathsPlain nixosReleaseCombined nixpkgsRelease nixpkgsReleaseFixed) =
  mapM_ putStrLn
    [ "store paths (plain)   : "
      ++ show (length storePathsPlain)
    , "nixos release-combined: "
      ++ show (length nixosReleaseCombined) ++ " derivation paths"
    , "ofborg nixpkgs release: "
      ++ show (length nixpkgsRelease) ++ " derivation paths / outputs"
    , "find-fixed-outputs.nix: "
      ++ show (length nixpkgsReleaseFixed) ++ " fixed outputs"
    , ""
    ]
