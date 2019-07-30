with import (fetchTarball {
  url = https://github.com/NixOS/nixpkgs/archive/4dd5c93998da55002fdec1c715c680531420381c.tar.gz;
  sha256 = "06paxakic36nbdnwkkb1094fzp3lpzxxb1r57gmb3py6pb6xrcnh";
}) {};
stdenv.mkDerivation {
  name = "nix-mirror";
  buildInputs = [ stack haskell.compiler.ghc822Binary glibcLocales zlib ];

  shellHook = ''
    export LANG=en_US.utf8
  '';
}
