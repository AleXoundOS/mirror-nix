{-# LANGUAGE FlexibleContexts #-}

module Main where

import Options.Applicative as OA
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad.Reader
import qualified Data.List as L (intercalate)
import Data.Maybe (mapMaybe)
import System.Directory (createDirectoryIfMissing)

import System.Nix.FixedOutput
import Download.Common (DownloadAppConfig(..))
import Download.BinaryCache
import Download.FixedOutputs


data Opts = Opts FilePath UseConduitRecurse [Command]
  deriving Show

data Command
  = BinaryCache FilePath
  | FixedOutputs FilePath [FoSource]
  deriving Show

data FoSource = FoSrcDerivations | FoSrcTarballs DryRun [Print]
  deriving (Eq, Show)

type UseConduitRecurse = Bool

type DryRun = Bool
data Print
  = PrintDrv
  | PrintHash | PrintMode | PrintName | PrintPath | PrintHashType | PrintUrls
  deriving (Eq, Show)


main :: IO ()
main = run mempty =<< customExecParser p opts
  where
    opts = info (helper <*> optsParser)
      ( fullDesc
        <> header "nix-mirror - download nix binary cache and fixed outputs" )
    p = defaultPrefs {prefShowHelpOnError = True}

run :: HashCache -> Opts -> IO ()
-- binary cache mirror command
run hc (Opts basePath useConduit (BinaryCache storePathsFile : cmds)) = do
  storePaths <- T.lines <$> T.readFile storePathsFile
  createDirectoryIfMissing True bcPath
  putStrLn
    ("starting binary cache download for \"" ++ storePathsFile ++ "\"")
  hc' <- runReaderT
         (downloadBinCacheForStorePaths hc storePaths)
         (DownloadAppConfig bcPath useConduit)
  putStrLn
    ("finished binary cache download for \"" ++ storePathsFile ++ "\"")
    -- work on the rest of the commands
    >> run hc' (Opts basePath useConduit cmds)
  where
    bcPath = basePath ++ "/cache"
-- fixed output derivations mirror command
run hc
  (Opts basePath useConduit
   (FixedOutputs foJsonFile (FoSrcDerivations : foSrcs) : cmds)) =
  let foPath = basePath ++ "/cache"
  in do
    createDirectoryIfMissing True foPath
    fixedOutputsInfos <- decodeFixedOutputsJsonFile foJsonFile
    putStrLn
      ("starting fixed output derivations download for \""
       ++ foJsonFile ++ "\"")
    hc' <- runReaderT
           (downloadFixedOutputDerivations hc fixedOutputsInfos)
           (DownloadAppConfig foPath useConduit)
    putStrLn
      ("finished fixed output derivations download for \""
       ++ foJsonFile ++ "\"")
    -- work on the rest of the fixed output sources (kinds)
    run hc' (Opts basePath useConduit
             (FixedOutputs foJsonFile foSrcs : cmds))
-- fixed output tarballs mirror command
run hc
  (Opts basePath useConduit
   (FixedOutputs foJsonFile (FoSrcTarballs isDry prints : foSrcs) : cmds)) =
  let foPath = basePath ++ "/tarballs"
  in do
    createDirectoryIfMissing True foPath
    fixedOutputsInfos <- decodeFixedOutputsJsonFile foJsonFile
    putStrLn
      ("starting fixed output tarballs download for \"" ++ foJsonFile ++ "\"")
    runReaderT
      (dlFoTbs isDry prints fixedOutputsInfos)
      (DownloadAppConfig foPath useConduit)
    putStrLn
      ("finished fixed output tarballs download for \"" ++ foJsonFile ++ "\"")
    -- work on the rest of the fixed output sources (kinds)
    run hc (Opts basePath useConduit
            (FixedOutputs foJsonFile foSrcs : cmds))
run hc (Opts basePath useConduit (FixedOutputs _ [] : cmds)) =
  -- work on the rest of the commands
  run hc (Opts basePath useConduit cmds)
run _ (Opts _ _ []) = return ()

-- | Download fixed output tarballs with logging (printing).
dlFoTbs :: (MonadReader DownloadAppConfig m, MonadIO m)
        => Bool -> [Print] -> [FixedOutputInfo] -> m ()
dlFoTbs isDry prints = mapM_ dlTbWithLog
  where
    dlTbWithLog foi = do
      liftIO $ putStr "[downloading] " >> printFixedOutputInfo prints foi
      unless isDry
        $ downloadFixedOutputTarball foi >>= liftIO . putStrLn . log' foi
    showErrors = ("\n---\n" ++) . L.intercalate "\n---\n" . map show
    log' _ (Nothing, []) =
      error "internal error: no file downloaded and no error"
    log' _ (Just fp, es) = "[saved] \"" ++ fp ++ "\"\n" ++
                           "while having some errors:\n" ++ showErrors es
    log' foi (Nothing, es) =
      "[error] could not download tarball for: " ++ T.unpack (_path foi)
      ++ showErrors es ++ "\n\n"

optsParser :: Parser Opts
optsParser = Opts
  <$> strOption
  (long "base-path" <> metavar "BASE_PATH" <> value "nix-mirror" <> showDefault
    <> help "Base path for mirror contents.")
  <*> switch
  (long "conduit-recurse"
    <> help "Use `leftover` conduit streaming mechanism for `NarInfo` \
            \recursion.")
  <*> some (hsubparser (binaryCacheCmd <> fixedOutputsCmd))

binaryCacheCmd :: Mod CommandFields Command
binaryCacheCmd = command "binaryCache"
  $ info (inputInfo <*> binaryCacheOpts)
  $ progDesc "Download Nix binary cache given `store-paths` file."
  where
    inputInfo = infoOption bcInputInfoMsg
      (long "input-help"
        <> help "Instructions for obtaining `store-paths` input file." )

binaryCacheOpts :: Parser Command
binaryCacheOpts = BinaryCache <$>
  strOption
  (long "store-paths" <> metavar "STORE_PATHS"
   <> help "Path to a \"store-paths\" file (a list of /nix/store/* paths).")

fixedOutputsCmd :: Mod CommandFields Command
fixedOutputsCmd = command "fixedOutputs"
  $ info (inputInfo <*> fixedOutputsOpts)
  $ progDesc "Download Nix fixed outputs given json array of derivations info."
  where
    inputInfo = infoOption foInputInfoMsg
      (long "input-help"
        <> help "Instructions for obtaining \
                \fixed output derivations json input file." )

bcInputInfoMsg :: String
bcInputInfoMsg =
  "1. Go to `https://nixos.org/channels/`.\n\
  \2. Download and decompress `store-paths.xz` file \
  \for the desired nixpkgs commit.\n"

fixedOutputsOpts :: Parser Command
fixedOutputsOpts = FixedOutputs <$>
  strOption
  (long "drvs-json" <> metavar "DRVS_JSON_FILE"
   <> help "Path to a json file produced with find-fixed-outputs.nix.")
  <*>
  some
  (
    flag' FoSrcDerivations
    (long "derivations"
     <> help "Download fixed output derivations (from cache.nixos.org), \
             \targeting at /nix/store/.")
    <|>
    flag' FoSrcTarballs
    (long "tarballs"
     <> help "Download the \"tarballs\" of fixed output derivations, \
             \building up a mirror of tarballs.nixos.org.")
    <*>
    switch
    (long "dry-run"
     <> help "Do not actually download. Useful in combination with --print-*.")
    <*>
    many prints
  )
  where
    prints =
      flag' PrintDrv
      (long "print-drv"       <> help "Print `drv` path (/nix/store/*.drv).")
      <|>
      flag' PrintHash
      (long "print-hash"      <> help "Print hashes.")
      <|>
      flag' PrintMode
      (long "print-mode"      <> help "Print mode: `flat` or `recursive`.")
      <|>
      flag' PrintName
      (long "print-name"      <> help "Print name of derivations.")
      <|>
      flag' PrintPath
      (long "print-path"      <> help "Print store path (/nix/store/*).")
      <|>
      flag' PrintHashType
      (long "print-hash-type" <> help "Print hash type, e.g. `sha1`.")
      <|>
      flag' PrintUrls
      (long "print-urls"      <> help "Print original source urls.")

foInputInfoMsg :: String
foInputInfoMsg =
  "1. Get `find-fixed-outputs.nix` script from `nix-mirror` git repository.\n\
  \2. Determine and set NIX_PATH $nixpkgs value for the derired commit.\n\
  \3. Use this command to produce the json file:\n\
  \$ nix-instantiate -I nixpkgs=$nixpkgs --eval --strict --json find-fixed-outputs.nix --arg expr 'import <nixpkgs/maintainers/scripts/all-tarballs.nix>' > nixpkgs-fixed-outputs.json\n\
  \\n\
  \Warning! This action needs approximately 8 GiB of RAM as of 2019-09."

-- | Print selected fields of a fixed output derivation.
printFixedOutputInfo :: [Print] -> FixedOutputInfo -> IO ()
printFixedOutputInfo ps (FixedOutputInfo drv hash mode name path type' urls) =
  let showAssoc =
        [ (PrintDrv,      "drv: "  ++ T.unpack drv)
        , (PrintHash,     "hash: " ++ T.unpack hash)
        , (PrintMode,     "mode: " ++ show mode)
        , (PrintName,     "name: " ++ T.unpack name)
        , (PrintPath,     "path: " ++ T.unpack path)
        , (PrintHashType, "type: " ++ show type')
        , (PrintUrls,     "urls: " ++ show urls)
        ]
  in putStrLn $ L.intercalate "; " $ mapMaybe (`lookup` showAssoc) ps
