#!/usr/bin/env bash

NIXPKGS_PATH=$1
OUTPUT_FILENAME=$2

DESCRIPTION=\
"An accessory script for getting a file containing a list (json encoded) `
`of fixed output derivations of the specified nixpkgs NIX_PATH.\n`
`NIXPKGS_PATH is either local path to nixpkgs contents or url to the tarball. `
`Actually this is the part after \`-I nixpkgs=\` when supplied to `
`\`nix-instantiate\`\n`
`The resulting file serves as an input for the \`nix-mirror\` program.\n`
`In future this functional should be integrated into \`nix-mirror\`.\n`
`Requirements: \`nix-instantiate\` and internet connection (if url given).\n`
`\nUsage: $(basename "$0") <NIXPKGS_PATH> [OUTPUT_FILENAME]\n`
`Example: ./gen-fixed-outputs-list.sh https://github.com/NixOS/nixpkgs/archive/4dd5c93998da55002fdec1c715c680531420381c.tar.gz"

if test -z "$NIXPKGS_PATH"
then
    echo "error: no nixpkgs NIXPKGS_PATH given!"
    echo -e "\n$DESCRIPTION"
    exit 1
else
    NIX_PATH="nixpkgs=$NIXPKGS_PATH"
    echo "using NIX_PATH=$NIX_PATH"
fi

if test -z "$OUTPUT_FILENAME"
then
    NIXPKGS_COMMIT_HASH=$(echo "$NIXPKGS_PATH" | rev | cut -d/ -f1 | rev | cut -d\. -f1)
    OUTPUT_FILENAME="nixpkgs-$NIXPKGS_COMMIT_HASH-fixed-outputs.json"
    echo "no output filename given, using \"$OUTPUT_FILENAME\""
fi

if test -e "$OUTPUT_FILENAME"
then
    echo "error: output file already exists, aborting!"
    exit 2
fi

nix-instantiate -I "$NIX_PATH" --eval --strict --json "find-fixed-outputs.nix" `
`--arg expr "import <nixpkgs/maintainers/scripts/all-tarballs.nix>" `
` > "$OUTPUT_FILENAME"
