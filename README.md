# nixos-mirror

There is an unsolved issue https://github.com/NixOS/nix/issues/2774 regarding
problems in mirroring nixos binary cache due to excessive complexity of the
`nix` tool for such a simple task. Meanwhile I've started this project.

# features
## binary cache mirroring
- downloading of `narinfo` with `nar.xz` with dependencies for specified store
  paths
  - dependencies are read from `narinfo` from the field `References`
- checksumming of downloaded `nar.xz`
- resumable downloads
- ensure files integrity after checksumming by utilizing btrfs snapshots
  capability
