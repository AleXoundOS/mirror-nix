module System.Nix.NixToolsProc
  ( Nixpkgs, NixArg, NixList(..)
  , nixInstantiate, nixInstantiateStrictBs
  , nixShowDerivationsRec, nixShowDerivationsRecB
  , nixStoreRealiseDrv, nixEnvQueryAvail
  , nixCopyPaths
  , nixSignPaths
  , mkNixStrList
  , nixShowDerivationsARec, nixShowDerivationsARecB
  , nixInstantiateAttrs, nixInstantiateAttrsB
  , ExitCode(..), Attr, NixInstAttrErr
  , batchList, batchListProg
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.HashMap.Strict (HashMap)
import Data.List (splitAt)
import Data.Text.Encoding (decodeUtf8)
import System.Exit (ExitCode(..))
import System.Process.Typed
import qualified Data.ByteString.Char8 as B (lines)
import qualified Data.Text as T (unpack)

import System.Nix.Derivation
import System.Nix.StoreNames
import System.Nix.EnvDrvInfo as E
import Utils (forceEitherStr)


type Nixpkgs = String
type NixArg = (String, String)
type Attr = String
type NixInstAttrErr = (Attr, ExitCode, ByteString)
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
         , ("NIXPKGS_ALLOW_INSECURE",           "1")
         , ("NIXPKGS_ALLOW_UNFREE",             "1")
         , ("NIXPKGS_ALLOW_UNSUPPORTED_SYSTEM", "1") -- TODO remove?
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

-- | @nix-instantiate@ the given attributes. Works in batch.
nixInstantiateAttrs :: Nixpkgs -> [NixArg] -> [FilePath] -> [Attr]
                    -> IO [Either NixInstAttrErr DrvPath]
nixInstantiateAttrs nixpkgs nixArgsTup files attrs = do
  (exitCode, out, err) <- readProcess (setEnvVars process)
  case (exitCode, attrs) of
    -- all given [Attr] instantiated successfully
    (ExitSuccess, _) ->
      return $ map (Right . decodeUtf8) $ B.lines $ toStrict out
    -- one Attr instantiation failed and it's the only Attr remaining
    (ExitFailure _, [attr]) ->
      return [Left (attr, exitCode, toStrict err)]
    -- at least one Attr failed and there several are given
    (ExitFailure _, _) ->
      (\(a, b) -> (++) <$> instantiatePart a <*> instantiatePart b)
      $ splitAt (length attrs `div` 2) attrs
  where
    process = proc "nix-instantiate"
      $ ["--quiet", "--quiet"]
      ++ mkNixPath nixpkgs ++ mkNixArgs nixArgsTup ++ files
      ++ concatMap mkAttrOption attrs
    mkAttrOption attr = ["-A", attr]
    instantiatePart = nixInstantiateAttrs nixpkgs nixArgsTup files

-- | @nix-instantiate@ the given attributes. Works in batch.
nixInstantiateAttrsB
  :: Int -> (Int -> Int -> IO ())
  -> Nixpkgs -> [NixArg] -> [FilePath] -> [Attr]
  -> IO [DrvPath]
nixInstantiateAttrsB n printProgress nixpkgs nixArgsTup files =
  batchListProg printProgress n ((map (either (error . show) id) <$>)
                                 . nixInstantiateAttrs nixpkgs nixArgsTup files)

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
nixShowDerivationsRecB n = batchListProg printProgress n nixShowDerivationsRec
  where
    printProgress left want =
      putStrLn $ "[" ++ show (want - left) ++ "/" ++ show want ++ "]"

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
nixStoreRealiseDrv :: DrvPath -> IO (Either (ExitCode, ByteString) [StoreName])
nixStoreRealiseDrv drvPath = do
  (exitCode, out, err) <- readProcess (setEnvVars process)
  case exitCode of
    ExitSuccess ->
      return $ Right $ map (forceEitherStr . stripParseStoreName . decodeUtf8)
      $ B.lines $ toStrict out
    ExitFailure _ ->
      return $ Left (exitCode, toStrict err)
  where
    process = proc "nix-store"
      $ "--realise" : "--quiet" : "--quiet" : [T.unpack drvPath]

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
batchListProg printProgress qtyAtOnce mFunc inpList = do
  out <- go inpList
  printProgress 0 (length inpList)
  return out
  where
    go [] = return mempty
    go ls = do
      printProgress (length ls) (length inpList)
      result <- mFunc (take qtyAtOnce ls)
      results <- go (drop qtyAtOnce ls)
      return (result <> results)
