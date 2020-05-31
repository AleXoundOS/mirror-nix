{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

module Download.Nix.All
  ( StorePathsSourcesInput(..), StorePathsSources(..)
  , getStorePathsSources, getAllPaths
  , printSourcesStats
  , instantiateEnvDrvs
  ) where

import Control.Applicative ((<|>))
import Data.Containers.ListUtils (nubOrd)
import Data.HashMap.Strict (HashMap)
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
getAllPaths :: StorePathsSources -> IO (Map StoreName (Maybe StoreExtra))
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
        [ ( forceEitherStr $ stripParseStoreName $ _path foInfo
          , Just (_drv foInfo, True) )
        | foInfo <- srcNixpkgsReleaseFixed
        ]

      drvPaths = nubOrd $ map E._drvPath srcNixpkgsRelease
  in do
    -- discover more store paths recursively from derivation paths
    pathsDiscovered <- drvMapToStoreMap
                       <$> nixShowDerivationsRecB 1000 drvPaths
    -- total discovery union, with always choosing Just DrvPath over Nothing
    return $ Map.unionsWith (<|>)
      [drvMapToStoreMap srcNixosReleaseCombined, pathsDiscovered, pathsDirect]

-- | All paths from @EnvDrvInfo@.
envDrvInfoPaths :: EnvDrvInfo -> [StoreTuple]
envDrvInfoPaths envDrvInfo = zip storeNames (repeat Nothing)
  where
    storeNames :: [StoreName]
    storeNames =
      map (forceEitherStr . stripParseStoreName . snd) $ _outputs envDrvInfo

-- | Extract all paths mentioned in any way from the given derivations. Not all
-- paths returned from @allDerivationPaths@ contain a genuine @DrvIsFixed@
-- value. So an alternative composition function is used for duplicate keys.
drvMapToStoreMap :: HashMap DrvPath DerivationP
                 -> Map StoreName (Maybe StoreExtra)
drvMapToStoreMap =
  Map.fromListWith updateExtra . concatMap drvToStore . HM.toList
  where
    drvToStore (drvPath, drv) =
      [ (storeName, Just (drvPath, isFixed))
      | (storeName, isFixed) <- allDerivationPaths drv ]
    updateExtra :: Maybe StoreExtra -> Maybe StoreExtra -> Maybe StoreExtra
    updateExtra old@(Just _) new@(Just _) =
      chooseOn ((== True) . snd) <$> old <*> new
    updateExtra old@(Just _) Nothing = old
    updateExtra Nothing new@(Just _) = new
    updateExtra Nothing Nothing = Nothing
    chooseOn :: (a -> Bool) -> a -> a -> a
    chooseOn p a b
      | p a = a
      | p b = b
      | otherwise = a

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
