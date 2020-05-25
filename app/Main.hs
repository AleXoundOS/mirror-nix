{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import Control.Monad.Reader
import Data.Foldable (sequenceA_)
import Data.Semigroup ((<>))
import Options.Applicative as OA
import System.Directory (createDirectoryIfMissing)
import System.Exit
import System.IO
import Text.Pretty.Simple (pPrint, pShowNoColor)
import qualified Data.ByteString.Char8 as B (readFile)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T (readFile, writeFile)
import qualified Data.Text.Lazy.IO as TL (writeFile)

import Download.Nix.All
import Download.Nix.Common (DownloadAppConfig(..))
import Download.Nix.NarInfos
import Download.Nix.Nars
import Download.Nix.Realise
import System.Nix.Derivation
import System.Nix.EnvDrvInfo (parseEnvDrvInfo)
import System.Nix.FixedOutput (decodeFixedOutputsJson)
import System.Nix.NarInfo
import System.Nix.StoreNames
import Utils (forceEitherStr)


data Opts = Opts
  { optCachePath     :: FilePath
  , optNarsDlChoice  :: NarsDownloadChoice
  , optRealiseChoice :: Maybe SignKey
  , optPathsDump     :: Maybe FilePath
  , optPathsMissDump :: Maybe FilePath
  , optNarInfoDump   :: Maybe FilePath
  , optNarDump       :: Maybe FilePath
  , optUseStreaming  :: Bool
  , optCacheBaseUrl  :: String
  , optNixpkgs       :: String
  , optSystems       :: [String]
  , optESrcInps      :: EitherSourcesInputs
  , optInstDrvsDump  :: Maybe FilePath
  , optLogFile       :: Maybe FilePath
  } deriving (Show)

data NarsDownloadChoice = NarsDlNew | NarsDlMissingToo | NarsDlNone
  deriving (Eq, Show)

data InputScriptOrData = InputScript FilePath | InputData FilePath
  deriving (Eq, Show)

type SignKey = String

data EitherSourcesInputs = EitherSourcesInputs
  { eitherInputChannel              :: Maybe FilePath
  , eitherInputNixosReleaseCombined :: Maybe InputScriptOrData
  , eitherInputNixpkgsRelease       :: Maybe InputScriptOrData
  , eitherInputNixpkgsReleaseFixed  :: Maybe InputScriptOrData
  } deriving (Show)


main :: IO ()
main = run =<< customExecParser p opts
  where
    opts = info (helper <*> optsParser)
      ( fullDesc
        <> header "nix-mirror-cache - \
                  \download specified piece of nix binary cache for http serve"
      )
    p = defaultPrefs {prefShowHelpOnError = True}

run :: Opts -> IO ()
run opts = do
  hSetBuffering stdout LineBuffering
  putStrLn "nix-mirror-cache start"
  when (all ((== Nothing) . ($ optESrcInps opts))
         [ fmap InputData . eitherInputChannel
         , eitherInputNixosReleaseCombined
         , eitherInputNixpkgsRelease
         , eitherInputNixpkgsReleaseFixed
         ]) $ do
    when (optNarsDlChoice opts /= NarsDlMissingToo) $ do
      putStrLn "no nix store paths sources given, exitting!"
      exitFailure
    undefined -- TODO: download missing Nars

  createDirectoryIfMissing True (optCachePath opts)

  putStrLn "---> calling nix tools"
  storePathsSourcesObtained <- getStorePathsSources
    $ mkStorePathsSourcesInput
    (optESrcInps opts) (optNixpkgs opts) (optSystems opts)

  putStrLn "---> nix tools obtained data stats:"
  printSourcesStats storePathsSourcesObtained{sourceChannel = []}

  putStrLn "---> reading data (if plain input files given)"
  storePathsSources <-
    replenishStorePathsSources storePathsSourcesObtained (optESrcInps opts)

  putStrLn "---> overall obtained data stats:"
  printSourcesStats storePathsSources

  putStrLn "---> instantiating derivations missing in /nix/store"
  (updatedEnvInfos, instDrvPaths) <-
    instantiateMissingEnvDrvs (optNixpkgs opts) (optSystems opts)
                              (sourceNixpkgsRelease storePathsSources)
  putStrLn
    $ "---> instantiated " ++ show (length instDrvPaths) ++ " derivations\n"
  sequenceA_ (flip T.writeFile (T.unlines instDrvPaths)
              <$> optInstDrvsDump opts)

  putStrLn "---> combining data"
  allStoreNames <-
    getAllPaths storePathsSources{sourceNixpkgsRelease = updatedEnvInfos}
  putStrLn
    $ "---> number of discovered (locally) store paths to get: "
    ++ show (Map.size allStoreNames)
  -- dumping all store paths
  sequenceA_ (flip T.writeFile
              (T.unlines $ map textStoreNamePath $ Map.keys allStoreNames)
               <$> optPathsDump opts
             )

  putStrLn "---> getting recursively all comprising narinfos"
  (GetNarInfosState narInfos missingPaths _ _ _) <-
    runReaderT (getNarInfos allStoreNames) dlAppConfig
  putStrLn
    $ "--->  store paths narinfo misses: " ++ show (length missingPaths)

  -- commented - causes a huge space leak!
  -- putStrLn $ "---> have " ++ show (length narInfos) ++ " narinfo's\n"

  -- TODO calculate estimated total size of nars

  -- dumping urls of all nars
  sequenceA_ (flip T.writeFile
              (T.unlines $ map _url narInfos)
               <$> optNarDump opts
             )

  -- dumping urls of all narinfos
  sequenceA_ (flip T.writeFile
               (T.unlines
                 $ map (mkNarInfoEndpFromStoreName . _storeName) narInfos)
               <$> optNarInfoDump opts
             )

  -- dumping store paths missing in remote binary cache
  sequenceA_ $ (<$> optPathsMissDump opts) $ \pathsMissDump -> do
    putStrLn $ "---> dumping store paths missing in " ++ optCacheBaseUrl opts
    TL.writeFile pathsMissDump $ pShowNoColor missingPaths

  -- realising missing store paths
  sequenceA_ $ (<$> optRealiseChoice opts) $ \signKey -> do
      putStrLn "---> realising store paths (that miss narinfo)"
      realiseState <- runReaderT
        (realiseAndCopyPaths signKey missingPaths) dlAppConfig
      pPrint realiseState
      putStrLn "---> finished store paths realisation"

  when doDlNars $ do
    putStrLn "---> getting nars (of every narinfo)"
    dlNarsState <-
      runReaderT (dlNars narInfos) dlAppConfig
    putStrLn "---> finished nars retrieval"
    putStrLn $ "---> got " ++ show (length $ stStored dlNarsState) ++ " nars"
    putStrLn $ "---> failed: " ++ show (Download.Nix.Nars.stFailed dlNarsState)

  putStrLn "---> finished binary cache download!"

  where
    dlAppConfig =
      DownloadAppConfig (optCachePath opts) (T.pack $ optCacheBaseUrl opts)
    doDlNars =
      case optNarsDlChoice opts of
        NarsDlNew        -> True
        NarsDlMissingToo -> True
        NarsDlNone       -> False

printStats :: [(GetPathStatus, (StoreName, Maybe DrvPath))] -> IO ()
printStats statuses = do
  putStrLn "---> obtained store paths stats"
  putStrLn $ "downloaded (ever) count: " ++ show downloadedCount
  putStrLn $ "built locally (realised) count: " ++ show realisedCount
  putStrLn $ "total (successful): " ++ show totalCount
  putStrLn $ "failed: " ++ show failedCount
  where
    downloadedCount = length $ filter ((== DownloadedFromServer) . fst) statuses
    realisedCount   = length $ filter ((== BuiltLocally) . fst) statuses
    totalCount      = downloadedCount + realisedCount
    failedCount     = length $ filter ((== StatusFailed) . fst) statuses

optsParser :: Parser Opts
optsParser = Opts
  <$> strOption
  (long "cache-path" <> metavar "CACHE_PATH"
   <> value "nix-cache-mirror" <> showDefault
   <> help "Base path for binary cache mirror contents")
  <*> narsDownloadChoiceParser
  <*> optional
  (strOption
   (long "sign-key" <> metavar "SIGN_KEY"
    <> help "Path to the private signing key for `nix sign-paths -k` \
            \needed during `nix copy` of realised paths")
  )
  <*> optional
  (strOption
    (long "dump-paths" <> metavar "STORE_PATHS_FILE"
     <> help "Dump target store paths (except recursive narinfo discovery)")
  )
  <*> optional
  (strOption
    (long "dump-paths-miss" <> metavar "DUMP_PATHS_MISS"
     <> help "Path to a file narinfo missing paths get written to. \
             \Useful to pass this file as store-paths input for realization.")
  )
  <*> optional
  (strOption
    (long "dump-narinfo-urls" <> metavar "NARINFO_URLS_FILE"
      <> help "Path to a file for storing all narinfos urls \
              \the program has downloaded.")
  )
  <*> optional
  (strOption
    (long "dump-nar-urls" <> metavar "NAR_URLS_FILE"
      <> help "Path to a file for storing all nar urls \
              \ the program has found before download.")
  )
  <*> switch
  (long "use-streaming"
   <> help "NOT IMPLEMENTED! Use `leftover` conduit streaming mechanism for \
           \binary cache `NarInfo` recursion (unknown which is best)")
  <*> strOption
   (long "cache-base-url" <> metavar "CACHE_BASE_URL"
     <> value "https://cache.nixos.org" <> showDefault)
  <*> strOption
   (long "nixpkgs" <> metavar "NIXPKGS" <> value "<nixpkgs>" <> showDefault
    <> help
     "The string after `-I nixpkgs=`")
  <*>
  (pure ["x86_64-linux"]
    <|> many
    (strOption
     (long "system" <> metavar "SYSTEM"
      <> help
       "Nix platform passed in `supportedSystems` list argument to expressions \
       \(multiple occurences of the option populate the list)")
    )
  )
  <*> eitherSourcesInputsParser
  <*> optional
  (strOption
    (long "inst-drvs-dump" <> metavar "INST_DRVS_DUMP"
      <> help "Path to a list of missing in /nix/store/ \
              \instantiated derivations."))
  <*> optional
  (strOption
    (long "log-file" <> metavar "LOG_FILE"
      <> help "Path to a log output file of this program. \
              \Currently disables stdout printing."))

narsDownloadChoiceParser :: Parser NarsDownloadChoice
narsDownloadChoiceParser =
  flag' NarsDlNew
  (long "nars-dl-new"
   <> help "Download nars discovered from the given inputs through narinfos")
  <|>
  flag' NarsDlMissingToo
  (long "nars-dl-missing"
   <> help "NOT IMPLEMENTED! Additionally download missing nars \
           \for scanned narinfos in cache dir")
  <|>
  flag' NarsDlNone
  (long "nars-dl-none"
   <> help "Do not download any nars")
  <|>
  pure NarsDlNew

eitherSourcesInputsParser :: Parser EitherSourcesInputs
eitherSourcesInputsParser = EitherSourcesInputs
  <$> optional
  (strOption
   (long "store-paths" <> short 's'
     <> metavar "STORE_PATHS"
     <> help
     "Path to a \"store-paths\" file containing a list of /nix/store/* paths")
  )
  <*> optional eitherInpParseNixosReleaseCombined
  <*> optional eitherInpParseNixpkgsRelease
  <*> optional eitherInpParseNixpkgsReleaseFixed

eitherInpParseNixosReleaseCombined :: Parser InputScriptOrData
eitherInpParseNixosReleaseCombined =
  InputScript <$> strOption
  (long "release-combined-nix" <> short 'r'
    <> metavar "RELEASE_COMBINED_NIX"
    <> help
    "Path to \"release-combined.nix\" \
    \(example: \"<nixpkgs/nixos/release-combined.nix>\")"
  )
  <|>
  InputData <$> strOption
  (long "release-combined-json" <> short 'R'
    <> metavar "RELEASE_COMBINED_DRVS"
    <> help
    "Path to a file with a json array of all derivation paths \
    \(a result of instantiating \"release-combined.nix\" \
    \and showing all derivations recursively; \
    \makes sense only if the whole derivation graph is present in\
    \ /nix/store)"
  )

eitherInpParseNixpkgsRelease :: Parser InputScriptOrData
eitherInpParseNixpkgsRelease =
  InputScript <$> strOption
   (long "ofborg-outpaths-nix" <> short 'o'
     <> metavar "OFBORG_OUTPATHS_NIX"
     <> help
     "Path to the outpaths nix script \
     \(originally from https://github.com/NixOS/ofborg; \
     \example: \"./ofborg-outpaths.nix\")"
   )
  <|>
  InputData <$> strOption
   (long "ofborg-outpaths-out" <> short 'O'
     <> metavar "OFBORG_OUTPATHS_OUT"
     <> help
    "Path to a file with a attrPath<->derivation<->output paths table \
    \(a result of running nix-env on outpaths script)"
   )

eitherInpParseNixpkgsReleaseFixed :: Parser InputScriptOrData
eitherInpParseNixpkgsReleaseFixed =
  InputScript <$> strOption
   (long "find-fixed-outputs-nix" <> short 'f'
     <> metavar "FIND_FIXED_OUTPUTS_NIX"
     <> help
     "Path to the \"find-fixed-outputs.nix\" script \
     \(see README for the origins; \
     \example: \"./find-fixed-outputs-nix\")"
   )
  <|>
  InputData <$> strOption
   (long "find-fixed-outputs-json" <> short 'F'
     <> metavar "FIND_FIXED_OUTPUTS_JSON"
     <> help
     "Path to a json file \
     \(a result of instantiating \"find-fixed-outputs.nix\")"
   )

mkStorePathsSourcesInput :: EitherSourcesInputs -> String -> [String]
                         -> StorePathsSourcesInput
mkStorePathsSourcesInput eitherSourcesInput nixpkgs systems =
  StorePathsSourcesInput
  { srcInputChannel = eitherInputChannel eitherSourcesInput
  , srcInputNixosReleaseCombined =
      eitherInputToMaybeFile
      $ eitherInputNixosReleaseCombined eitherSourcesInput
  , srcInputNixpkgsRelease =
      eitherInputToMaybeFile
      $ eitherInputNixpkgsRelease eitherSourcesInput
  , srcInputNixpkgsReleaseFixed =
      eitherInputToMaybeFile
      $ eitherInputNixpkgsReleaseFixed eitherSourcesInput
  , srcInputNixpkgs = nixpkgs
  , srcInputSystems = systems
  }

eitherInputToMaybeFile :: Maybe InputScriptOrData -> Maybe FilePath
eitherInputToMaybeFile (Just (InputScript fp)) = Just fp
eitherInputToMaybeFile (Just (InputData _))    = Nothing
eitherInputToMaybeFile Nothing = Nothing

replenishStorePathsSources :: StorePathsSources -> EitherSourcesInputs
                           -> IO StorePathsSources
replenishStorePathsSources
  (StorePathsSources
    srcChannel srcNixosReleaseCombined srcNixpkgsRelease srcNixpkgsReleaseFixed)
  (EitherSourcesInputs
    _eChannel  eNixosReleaseCombined   eNixpkgsRelease   eNixpkgsReleaseFixed) =
  StorePathsSources
  <$> pure   srcChannel
  <*> ifData srcNixosReleaseCombined eNixosReleaseCombined readJsonDrvs
  <*> ifData srcNixpkgsRelease       eNixpkgsRelease       readEnvDrvInfo
  <*> ifData srcNixpkgsReleaseFixed  eNixpkgsReleaseFixed  readFixedOutputInfo
  where
    ifData :: (Monoid a, Eq a)
      => a -> Maybe InputScriptOrData -> (FilePath -> IO a) -> IO a
    ifData have
      | have == mempty = caseHaveEmpty
      | otherwise      = caseHaveSmth have
    caseHaveEmpty (Just (InputData  fp)) readF = readF fp
    caseHaveEmpty (Just (InputScript _)) _     = error "missing script result"
    caseHaveEmpty Nothing _ = mempty
    caseHaveSmth  have (Just (InputScript _)) _ = return have
    caseHaveSmth _have (Just (InputData   _)) _ =
      error "both kinds of input are present!"
    caseHaveSmth have Nothing                _ = return have
    readJsonDrvs = fmap forceEitherStr . eitherDecodeFileStrict'
    readEnvDrvInfo = fmap (map parseEnvDrvInfo . T.lines) . T.readFile
    readFixedOutputInfo = fmap decodeFixedOutputsJson . B.readFile
