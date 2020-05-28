{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

module Download.Nix.All
  ( StorePathsSourcesInput(..), StorePathsSources(..)
  , GetPathStatus(..)
  , getStorePathsSources, getAllPaths
  , printSourcesStats
  , instantiateEnvDrvs
  ) where

import Control.Applicative ((<|>))
import Data.Containers.ListUtils (nubOrd)
import Data.HashMap.Strict (HashMap)
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

import System.Nix.Derivation (DerivationP) -- lol ?
import System.Nix.Derivation hiding (DerivationP(..))
import System.Nix.EnvDrvInfo as E
import System.Nix.FixedOutput as F
import System.Nix.NixToolsProc
import System.Nix.StoreNames
import System.Nix.StoreTuple
import Utils (forceEitherStr)


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
  { sourceChannel              :: ![StoreTuple]
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
    <$> maybe' readStoreTuple                           fpChannel
    <*> maybe' (nixShowDerivationsARec nixpkgs args []) fpNixosReleaseCombined
    <*> maybe' (nixEnvQueryAvail nixpkgs args . (:[]))  fpNixpkgsRelease
    <*> maybe' (instantiateFixedOutputs nixpkgs args)   fpNixpkgsReleaseFixed
  where
    maybe' f (Just a) = f a
    maybe' _ Nothing = return mempty
    args = [("supportedSystems", unNixList $ mkNixStrList systemsList)]

instantiateEnvDrvs :: Nixpkgs -> [String] -> [EnvDrvInfo]
                   -> IO [Either NixInstAttrErr EnvDrvInfo]
instantiateEnvDrvs nixpkgs systemsList = batchInstantiateInfos
  where
    batchInstantiateInfos
      :: [EnvDrvInfo] -> IO [Either NixInstAttrErr EnvDrvInfo]
    batchInstantiateInfos =
      batchListProg printProgress 100 (nixInstUpdEnvAttrs nixpkgs systemsList)
    printProgress left want = putStrLn
      $ "[" ++ show (want - left) ++ "/" ++ show want ++ "]"

nixInstUpdEnvAttrs :: Nixpkgs -> [String] -> [EnvDrvInfo]
                   -> IO [Either NixInstAttrErr EnvDrvInfo]
nixInstUpdEnvAttrs nixpkgs systemsList envDrvInfos = do
  instDrvPaths <- nixInstantiateAttrs
    nixpkgs args ["<nixpkgs/pkgs/top-level/release.nix>"] attrs
  if length instDrvPaths == length envDrvInfos
    then return
         [ updateEnvInfo envInfo <$> eDrvPath
         | (envInfo, eDrvPath) <- zip envDrvInfos instDrvPaths
         ]
    else error
         $ "instantiated derivations count /= attrs count\n"
         ++ show envDrvInfos ++ "\n"
         ++ show instDrvPaths
  where
    args = [("supportedSystems", unNixList $ mkNixStrList systemsList)]
    attrs = map (T.unpack . _attrPath) envDrvInfos
    updateEnvInfo envInfo drvPath = envInfo{_drvPath = drvPath}

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

      channelPathsMap = Map.fromList srcChannel

      nixpkgsReleasePathsMap = Map.fromList
        $ concatMap envDrvInfoPaths srcNixpkgsRelease

      nixpkgsReleaseFixedPathsMap = Map.fromList
        $ map (\foInfo -> ( forceEitherStr $ stripParseStoreName $ _path foInfo
                          , Just $ _drv foInfo )
              ) srcNixpkgsReleaseFixed

      drvPaths = nubOrd $ map E._drvPath srcNixpkgsRelease
  in do
    -- discover more store paths recursively from derivation paths
    pathsDiscovered <- drvMapToStoreMap
                       <$> nixShowDerivationsRecB 1000 drvPaths
    -- total discovery union, with always choosing Just DrvPath over Nothing
    return $ Map.unionsWith (<|>)
      [drvMapToStoreMap srcNixosReleaseCombined, pathsDiscovered, pathsDirect]

-- | All paths from @EnvDrvInfo@ with @StoreName@<->@DrvPath@ assoc normalized.
envDrvInfoPaths :: EnvDrvInfo -> [StoreTuple]
envDrvInfoPaths envDrvInfo = map (, Just $ _drvPath envDrvInfo) outputs
  where
    outputs :: [StoreName]
    outputs =
      map (forceEitherStr . stripParseStoreName . snd) $ _outputs envDrvInfo

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
