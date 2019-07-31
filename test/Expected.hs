{-# LANGUAGE OverloadedStrings #-}

module Expected where

import System.Nix.NarInfo

rightNarInfoYaml1 :: Either String NarInfo
rightNarInfoYaml1 = Right
  NarInfo
  {_storeHash = "003jhgnff2dwnz4j23wsqhzx9mdbxrqx"
  , _url = "nar/1an2kzz2gcwwchf6d8y2z70j1sy4r3m7wjlq10ia90dbys9ps1f3.nar.xz"
  , _compression = CompXz
  , _fileHash = "1an2kzz2gcwwchf6d8y2z70j1sy4r3m7wjlq10ia90dbys9ps1f3"
  , _fileSize = 32541236
  , _narHash = "sha256:1rq4cpf6adc9lqa5l4qnawpicd0ww36grzxl60a6wj3rjn0jm7d0"
  , _narSize = 125828016
  , _references = ["37a8nkijf2vy350q3lxhjmxwdnpm6lnf"
                  ,"681354n3k44r8z90m35hm8945vsp95h1"
                  ,"7mrq5v4nnqfkv1ka7a0kdw9mzvqs5w3c"
                  ,"9429azblcx1y9apas3azxxd38imcmsy0"
                  ,"iiymx8j7nlar3gc23lfkcscvr61fng8s"
                  ,"l3fbpz66yfjvamy0bi9fjlhwpp57g14y"
                  ,"sr4253np2gz2bpha4gn8gqlmiw604155"
                  ,"zx64bk34mqmgfrkbx6ls4hzzxzckah49"]
  , _deriver = Just "hvgi75bvyzj2zn73qyxxxi788lh3kncr-gcc-7.4.0.drv"
  , _sig = "cache.nixos.org-1:\
           \fitLERVREZX0cozJ4HlJxwB2dVRTT6ZipTyAoAzEpcDaBiV3J2xU+wEKHkspm9hR\
           \ISgAlJ/iOzjFy781/b6eCw=="}

rightNarInfoYaml2 :: Either String NarInfo
rightNarInfoYaml2 = (\x -> x {_references = []}) <$> rightNarInfoYaml1

rightNarInfoYaml3 :: Either String NarInfo
rightNarInfoYaml3 = (\x -> x {_deriver = Nothing}) <$> rightNarInfoYaml2

leftNarInfo1 :: Either String NarInfo
leftNarInfo1 = Left "Aeson exception:\nError in $: NarInfo YAML parsing Error! Invalid reference in: 003jhgnff2dwnz4j23wsqhzx9mdbxrqx-gcc-7.4.0 37a8nkijf2vy350q3lxhjmxwdnpm6lnf-isl-0.17.1 681354n3k44r8z90m35hm8945vsp95h1-glibc-2.27 7mrq5v4nnqfkv1ka7a0kdw9mzvqs5w3c-gmp-6.1.2 9429azblcx1y9apas3azxxd38imcmsy0-mpfr-4.0.2 iiymx8j7nlar3gc2lfkcscvr61fng8s-zlib-1.2.11 l3fbpz66yfjvamy0bi9fjlhwpp57g14y-gcc-7.4.0-lib sr4253np2gz2bpha4gn8gqlmiw604155-glibc-2.27-dev zx64bk34mqmgfrkbx6ls4hzzxzckah49-libmpc-1.1.0"

leftNarInfo2 :: Either String NarInfo
leftNarInfo2 = Left "Aeson exception:\nError in $: NarInfo YAML parsing Error! Unexpected `Compression` type read from Narinfo: zstd"

leftNarInfo3 :: Either String NarInfo
leftNarInfo3 = Left "Aeson exception:\nError in $: NarInfo YAML parsing Error! Invalid store path: /nix/storage/003jhgnff2dwnz4j23wsqhzx9mdbxrqx-gcc-7.4.0"
