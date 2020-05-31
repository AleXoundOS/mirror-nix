{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TupleSections #-}

module Download.Nix.All
  ( StorePathsSourcesInput(..), StorePathsSources(..)
  , getStorePathsSources, getAllPaths
  , printSourcesStats
  , instantiateEnvDrvs
  ) where

import Control.Monad (filterM, (<=<))
import Data.Either (lefts, rights)
import Data.HashMap.Strict (HashMap)
import Data.List (intercalate, partition)
import Data.Map.Strict (Map)
import System.Directory (doesFileExist)
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

instantiateEnvDrvs :: Bool -> Nixpkgs -> [String] -> [EnvDrvInfo]
                   -> IO ([EnvDrvInfo], [NixInstAttrErr], String)
instantiateEnvDrvs force nixpkgs systemsList =
  batchInstantiateInfos <=< filterEnvInfos force
  where
    batchInstantiateInfos :: [EnvDrvInfo]
                          -> IO ([EnvDrvInfo], [NixInstAttrErr], String)
    batchInstantiateInfos envDrvInfos = do
      instDrvResults <- batchListProg
        printProgress 100 (nixInstEnvAttrs nixpkgs systemsList) envDrvInfos
      let (good, bad) = check envDrvInfos instDrvResults
      return (good, lefts instDrvResults, showCheck bad)
    printProgress left want =
      putStrLn $ "[" ++ show (want - left) ++ "/" ++ show want ++ "]"
    filterEnvInfos True = return
    filterEnvInfos False =
      filterM (fmap not . doesFileExist . T.unpack . _drvPath)
    -- returns ((expected, got)) tuple
    check :: [EnvDrvInfo] -> [Either NixInstAttrErr DrvPath]
          -> ([EnvDrvInfo], [(EnvDrvInfo, DrvPath)])
    check envDrvInfos instDrvResults =
      let (good, bad) =
            partition (uncurry ((==) . _drvPath))
            $ rights
            $ zipWith (\a b -> (a,) <$> b) envDrvInfos instDrvResults
      in (map fst good, bad)
    showCheck [] = ""
    showCheck unmatched = "instantiated derivations do not match expected\n"
      ++ intercalate ['\n'] (map showSingleUnmatched unmatched)
    showSingleUnmatched (envDrvInfoUnmatched, drvPathGot) =
      "expected: " ++ show envDrvInfoUnmatched ++ "\n"
      ++ "got: " ++ show drvPathGot ++ "\n"


nixInstEnvAttrs :: Nixpkgs -> [String] -> [EnvDrvInfo]
                -> IO [Either NixInstAttrErr DrvPath]
nixInstEnvAttrs nixpkgs systemsList envDrvInfos = nixInstantiateAttrs
  nixpkgs args ["<nixpkgs/pkgs/top-level/release.nix>"] attrs
  where
    args = [("supportedSystems", unNixList $ mkNixStrList systemsList)]
    attrs = map (T.unpack . _attrPath) envDrvInfos

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
          , Just (_drv foInfo, DrvIsFixed) )
        | foInfo <- srcNixpkgsReleaseFixed
        ]

      drvPaths =
        map E._drvPath srcNixpkgsRelease
        -- in case we are given plain derivation paths as well
        ++ [ drvPath
           | (_, Just (drvPath, isFixed)) <- srcChannel, isFixed /= DrvIsFixed]
  in do
    -- discover more store paths recursively from derivation paths
    pathsDiscovered <- drvMapToStoreMap
                       <$> nixShowDerivationsRecB 1000 drvPaths
    -- total discovery union, with always choosing Just DrvPath over Nothing
    return $ Map.unionsWith updateExtra
      [drvMapToStoreMap srcNixosReleaseCombined, pathsDiscovered, pathsDirect]

-- | All paths from @EnvDrvInfo@.
envDrvInfoPaths :: EnvDrvInfo -> [StoreTuple]
envDrvInfoPaths envDrvInfo = map mkStoreTuple $ _outputs envDrvInfo
  where
    mkStoreTuple (_, outPath) =
      ( forceEitherStr $ stripParseStoreName outPath
      , Just (_drvPath envDrvInfo, DrvFixedUnknown) )

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
  chooseOn ((/= DrvFixedUnknown) . snd) <$> old <*> new
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
