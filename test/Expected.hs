{-# LANGUAGE OverloadedStrings #-}

module Expected where

import Data.HashMap.Strict (HashMap, fromList)

import System.Nix.Derivation
import System.Nix.NarInfo
import System.Nix.StoreNames


justNarInfoYaml1 :: Maybe NarInfo
justNarInfoYaml1 = Just
    ( NarInfo
        { _storeName = StoreName
            { unStoreName =
                ( "003jhgnff2dwnz4j23wsqhzx9mdbxrqx"
                , "gcc-7.4.0"
                )
            }
        , _url = "nar/1an2kzz2gcwwchf6d8y2z70j1sy4r3m7wjlq10ia90dbys9ps1f3.nar.xz"
        , _compression = CompXz
        , _fileHash = "1an2kzz2gcwwchf6d8y2z70j1sy4r3m7wjlq10ia90dbys9ps1f3"
        , _fileSize = 32541236
        , _narHash = "sha256:1rq4cpf6adc9lqa5l4qnawpicd0ww36grzxl60a6wj3rjn0jm7d0"
        , _narSize = 125828016
        , _references =
            [ StoreName
                { unStoreName =
                    ( "37a8nkijf2vy350q3lxhjmxwdnpm6lnf"
                    , "isl-0.17.1"
                    )
                }
            , StoreName
                { unStoreName =
                    ( "681354n3k44r8z90m35hm8945vsp95h1"
                    , "glibc-2.27"
                    )
                }
            , StoreName
                { unStoreName =
                    ( "7mrq5v4nnqfkv1ka7a0kdw9mzvqs5w3c"
                    , "gmp-6.1.2"
                    )
                }
            , StoreName
                { unStoreName =
                    ( "9429azblcx1y9apas3azxxd38imcmsy0"
                    , "mpfr-4.0.2"
                    )
                }
            , StoreName
                { unStoreName =
                    ( "iiymx8j7nlar3gc23lfkcscvr61fng8s"
                    , "zlib-1.2.11"
                    )
                }
            , StoreName
                { unStoreName =
                    ( "l3fbpz66yfjvamy0bi9fjlhwpp57g14y"
                    , "gcc-7.4.0-lib"
                    )
                }
            , StoreName
                { unStoreName =
                    ( "sr4253np2gz2bpha4gn8gqlmiw604155"
                    , "glibc-2.27-dev"
                    )
                }
            , StoreName
                { unStoreName =
                    ( "zx64bk34mqmgfrkbx6ls4hzzxzckah49"
                    , "libmpc-1.1.0"
                    )
                }
            ]
        , _deriver = Just "hvgi75bvyzj2zn73qyxxxi788lh3kncr-gcc-7.4.0.drv"
        , _sig = "cache.nixos.org-1:fitLERVREZX0cozJ4HlJxwB2dVRTT6ZipTyAoAzEpcDaBiV3J2xU+wEKHkspm9hRISgAlJ/iOzjFy781/b6eCw=="
        }
    )

justNarInfoYaml2 :: Maybe NarInfo
justNarInfoYaml2 = (\x -> x {_references = []}) <$> justNarInfoYaml1

justNarInfoYaml3 :: Maybe NarInfo
justNarInfoYaml3 = (\x -> x {_deriver = Nothing}) <$> justNarInfoYaml2

noNarInfo1 :: Maybe NarInfo
noNarInfo1 = Nothing

noNarInfo2 :: Maybe NarInfo
noNarInfo2 = Nothing

noNarInfo3 :: Maybe NarInfo
noNarInfo3 = Nothing

jsonDerivation1 :: Maybe (HashMap DrvPath Derivation)
jsonDerivation1 = Just $ fromList
  [
      ( "/nix/store/asjiw2q6lny0qrz7y9alvamjskx2bl6s-base64_js___base64_js_1.2.3.tgz.drv"
      , Derivation
          { drvOutputs =
              [
                  ( "out"
                  , StoreName
                      { unStoreName =
                          ( "nf85iwaj7m1y69ci2niszg05pqp91wgp"
                          , "base64_js___base64_js_1.2.3.tgz"
                          )
                      }
                  )
              ]
          , drvInputSrcs = [ "/nix/store/4snpxiw4s2f0nqn6vl9d7p1jqbiar6iy-builder.sh" ]
          , drvInputDrvs = fromList
              [
                  ( "/nix/store/hlaih3x0mabh6n1cgcw2ka4m1mm7yp2c-bash-4.4-p23.drv"
                  , [ "out" ]
                  )
              ,
                  ( "/nix/store/vxlyv4yf8b08ddricyv9678dn8p5bbgz-curl-7.65.3.drv"
                  , [ "dev" ]
                  )
              ,
                  ( "/nix/store/73ijll441w7rrv50jks04v53dm3p2zfq-mirrors-list.drv"
                  , [ "out" ]
                  )
              ,
                  ( "/nix/store/jxgsszzgd68cy259bpdqlzr0nw6h15nf-stdenv-linux.drv"
                  , [ "out" ]
                  )
              ]
          , drvBuilder = Just "/nix/store/rm1hz1lybxangc8sdl7xvzs5dcvigvf7-bash-4.4-p23/bin/bash"
          , drvEnvPaths =
              [ StoreName
                  { unStoreName =
                      ( "g1hi9brlckc20w1fvlfk59bcgl0z6s21"
                      , "curl-7.65.3-dev"
                      )
                  }
              , StoreName
                  { unStoreName =
                      ( "yvparsffmc3m34afivqrchqfb3zjwpkm"
                      , "mirrors-list"
                      )
                  }
              , StoreName
                  { unStoreName =
                      ( "3g2pkmc1s9ycjaxaqc5hrzmq05r5ywbi"
                      , "stdenv-linux"
                      )
                  }
              , StoreName
                  { unStoreName =
                      ( "nf85iwaj7m1y69ci2niszg05pqp91wgp"
                      , "base64_js___base64_js_1.2.3.tgz"
                      )
                  }
              ]
          }
      )
  ]

noDerivation1 :: Maybe (HashMap DrvPath Derivation)
noDerivation1 = Nothing
