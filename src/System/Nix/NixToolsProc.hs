module System.Nix.NixToolsProc
  ( Nixpkgs, NixArg, NixList(..)
  , nixInstantiate, nixInstantiateStrictBs
  , nixShowDerivationRec, nixShowDerivationsRec
  , nixStoreRealiseDrvs, nixEnvQueryAvail
  , nixCopyPaths
  , nixSignPaths
  , mkNixStrList
  , nixInstantiateAttrs
  , batchList
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.HashMap.Strict (HashMap)
import Data.Text.Encoding (decodeUtf8)
import System.Process.Typed
import qualified Data.ByteString.Char8 as B (lines)
import qualified Data.Text as T (unpack)

import System.Nix.Derivation
import System.Nix.StoreNames
import System.Nix.EnvDrvInfo as E
import Utils (forceEitherStr)


type Nixpkgs = String
type NixArg = (String, String)
newtype NixList = NixList {unNixList :: String}


gcInitialHeapSize :: String
gcInitialHeapSize = "6g"

mkNixStrList :: [String] -> NixList
mkNixStrList = NixList . bracketify . unwords . map show
  where
    bracketify str = "[ " ++ str ++ " ]"

setEnvVars
  :: ProcessConfig stdin stdout stderr -> ProcessConfig stdin stdout stderr
setEnvVars =
  setEnv [ ("GC_INITIAL_HEAP_SIZE", gcInitialHeapSize)
         , ("NIXPKGS_ALLOW_INSECURE", "1")
         , ("NIXPKGS_ALLOW_UNFREE", "1") ]

mkNixPath :: Nixpkgs -> [String]
mkNixPath nixpkgs = ["-I", "nixpkgs=" ++ nixpkgs]

mkNixArgs :: [NixArg] -> [String]
mkNixArgs = concatMap (\(name, value) -> ["--arg", name, value])

-- | @nix-instantiate@ the given nix scripts.
nixInstantiate :: Nixpkgs -> [NixArg] -> [FilePath] -> IO [DrvPath]
nixInstantiate nixpkgs nixArgsTup files = map decodeUtf8 . B.lines . toStrict
  <$> (print process >> readProcessStdout_ (setEnvVars process))
  where
    process = proc "nix-instantiate"
      $ ["--quiet", "--quiet"]
      ++ mkNixPath nixpkgs ++ mkNixArgs nixArgsTup ++ files

-- TODO quiet
-- | @nix-instantiate@ the given nix scripts.
nixInstantiateAttrs :: Nixpkgs -> [NixArg] -> [FilePath] -> [String]
                    -> IO [DrvPath]
nixInstantiateAttrs nixpkgs nixArgsTup files attrs =
  map decodeUtf8 . B.lines . toStrict . snd
  <$> (print process >> readProcessStdout (setEnvVars process))
  where
    process = proc "nix-instantiate"
      $ ["--quiet", "--quiet"]
      ++ mkNixPath nixpkgs ++ mkNixArgs nixArgsTup ++ files
      ++ concatMap mkAttrOption attrs
    mkAttrOption attr = ["-A", attr]

-- | @nix-instantiate@ the given nix scripts with strict evaluation and JSON
-- output.
nixInstantiateStrictBs :: Nixpkgs -> [NixArg] -> [FilePath] -> IO ByteString
nixInstantiateStrictBs nixpkgs nixArgsTup files = toStrict
  <$> (print process >> readProcessStdout_ (setEnvVars process))
  where
    process = proc "nix-instantiate"
      $ ["--quiet", "--quiet", "--eval", "--strict", "--json"]
      ++ mkNixPath nixpkgs ++ mkNixArgs nixArgsTup ++ files

-- | @nix-env@ with Nix expression to obtain derivations.
nixEnvQueryAvail :: Nixpkgs -> [NixArg] -> [FilePath] -> IO [EnvDrvInfo]
nixEnvQueryAvail nixpkgs nixArgsTup files =
  map (parseEnvDrvInfo . decodeUtf8) . B.lines . toStrict
  <$> (print process >> readProcessStdout_ (setEnvVars process))
  where
    process = proc "nix-env"
      $ ["--quiet", "--quiet", "-qa"
        , "--no-name", "--attr-path", "--drv-path", "--out-path" ]
      ++ mkNixPath nixpkgs ++ mkNixArgs nixArgsTup
      ++ concatMap (\file -> ["-f", file]) files

-- | @nix show-derivation --recursive@ the given derivation path.
nixShowDerivationRec :: DrvPath -> IO (HashMap DrvPath DerivationP)
nixShowDerivationRec drvPath = either error id . eitherDecodeStrict' . toStrict
  <$> readProcessStdout_ process
  where
    process = proc "nix"
      ["show-derivation", "--recursive", T.unpack drvPath]

nixShowDerivationsRec :: HashMap DrvPath DerivationP -> IO (HashMap DrvPath DerivationP)
nixShowDerivationsRec = undefined -- TODO batch `xargs` style calls

-- | @nix-store --realise@.
nixStoreRealiseDrvs :: [DrvPath] -> IO [StoreName]
nixStoreRealiseDrvs drvPaths =
  map (forceEitherStr . stripParseStoreName . decodeUtf8)
  . B.lines . toStrict . snd
  <$> readProcessStdout process
  where
    process = proc "nix-store"
      $ "--realise" : map T.unpack drvPaths

-- | @nix copy@ store paths to file:// store-uri.
nixCopyPaths :: [FilePath] -> FilePath -> IO ()
nixCopyPaths storePaths destFp = runProcess_ process
  where
    process = proc "nix"
      $ ["copy", "--to", "file://" ++ destFp] ++ storePaths

-- | @nix sign-paths -r@ store paths to file:// store-uri.
nixSignPaths :: [FilePath] -> FilePath -> IO ()
nixSignPaths storePaths key = runProcess_ process
  where
    process = proc "nix"
      $ ["sign-paths", "-r", "-k", key] ++ storePaths

-- | Batch IO list processing.
batchList :: Monad m => ([a] -> m [b]) -> Int -> [a] -> m [b]
batchList _ _ [] = return []
batchList mFunc qtyAtOnce inpList = go inpList
  where
    go [] = return []
    go ls = do
      result <- mFunc (take qtyAtOnce ls)
      results <- go (drop qtyAtOnce ls)
      return (result ++ results)
