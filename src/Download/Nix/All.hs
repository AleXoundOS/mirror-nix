module Download.Nix.All
  (
  ) where

import Data.Text (Text)
import Data.Containers.ListUtils (nubOrd)

import Download.Nix.BinaryCache
import System.Nix.Derivation
import System.Nix.EnvDrvInfo as E
import System.Nix.FixedOutput as F
import System.Nix.NixToolsProc
import System.Nix.StorePath


data StorePathsSources = StorePathsSources
  -- | store-paths.xz
  { sourceChannel              :: [StorePath]
  -- | nix-instantiate nixos/release-combined.nix
  , sourceNixosReleaseCombined :: [Derivation]
  -- | nix-env -qaP .\/outpaths.nix (from ofborg) @ pkgs\/top-level\/release.nix
  , sourceNixpkgsRelease       :: [EnvDrvInfo]
  -- | maintainers\/scripts\/all-tarballs.nix
  , sourceNixpkgsReleaseFixed  :: [FixedOutputInfo]
  }

data Path = Path StorePath (Maybe Derivation)


getAllStorePaths :: StorePathsSources -> IO [StorePath]
getAllStorePaths (StorePathsSources
                  srcChannel
                  srcNixosReleaseCombined
                  srcNixpkgsRelease
                  srcNixpkgsReleaseFixed) =
  let pathsDirect = concat
        [                                 srcChannel
        , concatMap allDerivationPaths    srcNixosReleaseCombined
        , concatMap (map snd . E._output) srcNixpkgsRelease
        , map       F._path               srcNixpkgsReleaseFixed
        ]
  in do
    -- discover more store paths recursively from derivation paths
    pathsDiscover <- concat <$> mapM allPathsFromDrvPath
      (concat
       [ map E._drvPath srcNixpkgsRelease
       , map F._drv     srcNixpkgsReleaseFixed
       ]
      )
    return $ nubOrd (pathsDirect ++ pathsDiscover)

allPathsFromDrvPath :: DrvPath -> IO [StorePath]
allPathsFromDrvPath =
  fmap (concatMap allPathsFromDerivation) . nixShowDerivationRec

allPathsFromDerivation :: Derivation -> [StorePath]
allPathsFromDerivation = undefined

-- downloadStorePathsCache :: [StorePath] -> IO [StorePath]
-- downloadStorePathsCache = downloadBinCacheForStorePaths
-- substract downloaded paths from [Derivation]?

downloadAllStorePaths :: StorePathsSources -> IO ()
downloadAllStorePaths = undefined

lookupDrvPath :: [Derivation] -> StorePath -> Maybe DrvPath
lookupDrvPath = undefined

-- getAllDrvPaths :: []
