let
  haskellNix = import (builtins.fetchTarball
    "https://github.com/input-output-hk/haskell.nix/archive/643c219a5ef0a4b2570c8d056fa827214b4a0b64.tar.gz") { };
  nixpkgsSrc = haskellNix.sources.nixpkgs-2003;
  nixpkgsArgs = haskellNix.nixpkgsArgs;
in { pkgs ? import nixpkgsSrc nixpkgsArgs }:
pkgs.haskell-nix.stackProject {
  src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; };
}
