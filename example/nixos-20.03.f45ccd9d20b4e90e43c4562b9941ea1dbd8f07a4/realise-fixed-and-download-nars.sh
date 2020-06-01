NIXPKGS="https://github.com/NixOS/nixpkgs/archive/f45ccd9d20b4e90e43c4562b9941ea1dbd8f07a4.tar.gz"
stack exec -- nix-mirror-cache `
`--nixpkgs="$NIXPKGS" `
`--dump-paths="paths.dump" `
`--dump-paths-miss="paths-miss.dump" `
`--dump-narinfo-urls="narinfo-urls.dump" --dump-nar-urls="nar-urls.dump" `
`--store-paths="store-paths" `
`--release-combined-json="instantiate-release-combined-recursive.json" `
`--ofborg-outpaths-out="nix-env-ofborg-outpaths.txt" `
`--find-fixed-outputs-json="find-fixed-outputs-pp.json" `
`--realise-log="realise.log" `
`--inst-fail-dump="inst-fail.dump" `
`--realise-fixed-only `
` $@ |& tee log.txt

