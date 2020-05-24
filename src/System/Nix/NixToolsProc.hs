module System.Nix.NixToolsProc
  ( Nixpkgs, NixArg, NixList(..)
  , nixInstantiate, nixInstantiateStrictBs
  , nixShowDerivationsRec, nixShowDerivationsRecB
  , nixStoreRealiseDrvs, nixEnvQueryAvail
  , nixCopyPaths
  , nixSignPaths
  , mkNixStrList
  , nixShowDerivationsARec, nixShowDerivationsARecB
  , nixInstantiateAttrs, nixInstantiateAttrsB
  , batchList, batchListProg
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
  setEnv [ ("GC_INITIAL_HEAP_SIZE",             gcInitialHeapSize)
         , ("NIXPKGS_ALLOW_BROKEN",             "1")
         , ("NIXPKGS_ALLOW_INSECURE",           "1")
         , ("NIXPKGS_ALLOW_UNFREE",             "1")
         , ("NIXPKGS_ALLOW_UNSUPPORTED_SYSTEM", "1")
         ]

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

-- TODO quiet?
-- | @nix-instantiate@ the given attributes. Works in batch.
nixInstantiateAttrs :: Nixpkgs -> [NixArg] -> [FilePath] -> [String]
                    -> IO [DrvPath]
nixInstantiateAttrs nixpkgs nixArgsTup files attrs =
  map decodeUtf8 . B.lines . toStrict
  <$> readProcessStdout_ (setEnvVars process)
  where
    process = proc "nix-instantiate"
      $ ["--quiet", "--quiet"]
      ++ mkNixPath nixpkgs ++ mkNixArgs nixArgsTup ++ files
      ++ concatMap mkAttrOption attrs
    mkAttrOption attr = ["-A", attr]

-- | @nix-instantiate@ the given attributes. Works in batch.
nixInstantiateAttrsB
  :: Int -> (Int -> Int -> IO ())
  -> Nixpkgs -> [NixArg] -> [FilePath] -> [String]
  -> IO [DrvPath]
nixInstantiateAttrsB n printProgress nixpkgs nixArgsTup files =
  batchListProg printProgress n (nixInstantiateAttrs nixpkgs nixArgsTup files)

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

-- | @nix show-derivation --recursive@ the given derivations paths.
nixShowDerivationsRec :: [DrvPath] -> IO (HashMap DrvPath DerivationP)
nixShowDerivationsRec drvPaths = forceEitherStr . eitherDecodeStrict' . toStrict
  <$> readProcessStdout_ process
  where
    process = proc "nix"
      $ ["show-derivation", "--recursive"] ++ map T.unpack drvPaths

-- | @nix show-derivation --recursive@ the given derivations paths.
nixShowDerivationsRecB :: Int -> [DrvPath] -> IO (HashMap DrvPath DerivationP)
nixShowDerivationsRecB n = batchList n nixShowDerivationsRec

-- | @nix show-derivation --recursive@ derivation paths of the given attributes.
nixShowDerivationsARec
  :: Nixpkgs -> [NixArg] -> [String] -> String
  -> IO (HashMap DrvPath DerivationP)
nixShowDerivationsARec nixpkgs nixArgsTup attrs file =
  forceEitherStr . eitherDecodeStrict' . toStrict
  <$> (print process >> readProcessStdout_ process)
  where
    process = proc "nix"
      $ ["show-derivation", "--recursive"]
      ++ mkNixPath nixpkgs ++ mkNixArgs nixArgsTup
      ++ ["-f", file] ++ attrs

-- | @nix show-derivation --recursive@ derivation paths of the given attributes.
-- Works in batch.
nixShowDerivationsARecB
  :: Int -> Nixpkgs -> [NixArg] -> String -> [String]
  -> IO (HashMap DrvPath DerivationP)
nixShowDerivationsARecB n nixpkgs nixArgsTup file =
  batchList n (\attrs -> nixShowDerivationsARec nixpkgs nixArgsTup attrs file)

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
batchList :: (Monad m, Monoid b) => Int -> ([a] -> m b) -> [a] -> m b
batchList = batchListProg (\_ _ -> return ())

-- | Batch IO list processing.
batchListProg :: (Monad m, Monoid b) =>
  (Int -> Int -> m ()) -> Int -> ([a] -> m b) -> [a] -> m b
batchListProg _ _ _ [] = return mempty
batchListProg printProgress qtyAtOnce mFunc inpList = go inpList
  where
    go [] = return mempty
    go ls = do
      result <- mFunc (take qtyAtOnce ls)
      printProgress (length ls) (length inpList)
      results <- go (drop qtyAtOnce ls)
      printProgress (length ls) (length inpList)
      return (result <> results)
