let mkPackage =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.0-20190626/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.8-20201021/packages.dhall sha256:55ebdbda1bd6ede4d5307fbc1ef19988c80271b4225d833c8d6fb9b6fb1aa6d8

in  upstream
  with
    polyform =
      mkPackage
        [ "newtype"
        , "ordered-collections"
        , "variant"
        , "profunctor"
        , "invariant"
        , "foreign-object"
        , "run"
        , "transformers"
        , "generics-rep"
        , "validation"
        , "foreign"
        ]
        "https://github.com/purescript-polyform/polyform.git"
        "master"
