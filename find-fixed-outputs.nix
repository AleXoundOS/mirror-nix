# Copyright (c) 2003-2019 Eelco Dolstra, Daiderd Jordan, Alexander Tomokhov and
# the Nixpkgs/NixOS contributors.
#
# This work is licensed under the terms of the MIT license.
# For a copy, see <https://opensource.org/licenses/MIT>.
#
# This expression returns a list of all fixed output derivations used by ‘expr’.
# eg.
# $ nix-instantiate find-fixed-outputs.nix --eval --strict --json --arg expr '(import <nixpkgs> {}).hello'

with import <nixpkgs> { };
with lib;

{ expr }:

let

  root = expr;

  uniqueSrcs = map (x: x.file) (genericClosure {
    startSet = map (file: { key = file.hash; inherit file; }) srcs;
    operator = const [ ];
  });

  srcs = map (drv: { drv = drv.drvPath;
                     hash = drv.outputHash;
                     type = drv.outputHashAlgo;
                     mode = drv.outputHashMode;
                     name = drv.name;
                     path = drv;
                     urls = if drv ? urls
                            then fixToSingleList drv.urls
                            else
                              if drv ? url
                              then fixToSingleList drv.url
                              else [];
                     }) drvDependencies;

  # Some nixpkgs commits contain derivations with different nesting level of
  # drv.url and drv.urls lists. E.g. 4dd5c93998da55002fdec1c715c680531420381c.
  fixToSingleList = x:
    if lib.isList x
    then
      if lib.length x > 0
      then lib.concatMap fixToSingleList x
      else []
    else [x];

  drvDependencies =
    filter
      (drv: drv.outputHash or "" != "")
      dependencies;

  dependencies = map (x: x.value) (genericClosure {
    startSet = map keyDrv (derivationsIn' root);
    operator = { key, value }: map keyDrv (immediateDependenciesOf value);
  });

  derivationsIn' = x:
    if !canEval x then []
    else if isDerivation x then optional (canEval x.drvPath) x
    else if isList x then concatLists (map derivationsIn' x)
    else if isAttrs x then concatLists (mapAttrsToList (n: v: addErrorContext
      "while finding fixed outputs in '${n}':" (derivationsIn' v)) x)
    else [];

  keyDrv = drv:
    if canEval drv.drvPath then { key = drv.drvPath; value = drv; } else { };

  immediateDependenciesOf = drv:
    concatLists (mapAttrsToList (n: v: derivationsIn v)
      (removeAttrs drv ["meta" "passthru"]));

  derivationsIn = x:
    if !canEval x then []
    else if isDerivation x then optional (canEval x.drvPath) x
    else if isList x then concatLists (map derivationsIn x)
    else [];

  canEval = val: (builtins.tryEval val).success;

in uniqueSrcs
