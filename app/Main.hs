module Main where

import Options.Applicative as OA
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Download


data Opts = Opts { path :: FilePath }


main :: IO ()
main = run =<< execParser opts
  where
    opts = info (optsParser <**> helper)
      ( fullDesc
        <> progDesc "Download complete nix binary cache for the given FILE \
                    \containing a list of store paths."
        <> header "nix-mirror - program for downloading nix binary cache" )

run :: Opts -> IO ()
run (Opts path') = T.lines <$> T.readFile path'
  >>= downloadNixBinCacheForStorePaths'

optsParser :: Parser Opts
optsParser = Opts
  <$> strArgument
  (metavar "FILE")
