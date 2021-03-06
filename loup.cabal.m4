name:                  loup
version:               VERSION
synopsis:              Amazon Simple Workflow Service Wrapper for Work Pools.
description:           Loup is a wrapper around Amazon Simple Workflow Service for Work Pools.
homepage:              https://github.com/swift-nav/loup
license:               MIT
license-file:          LICENSE
author:                Swift Navigation Inc.
maintainer:            Mark Fine <dev@swiftnav.com>
copyright:             Copyright (C) 2017 Swift Navigation, Inc.
category:              Network, AWS, Cloud, Distributed Computing
build-type:            Simple
cabal-version:         >= 1.22

source-repository head
  type:                git
  location:            git@github.com:swift-nav/loup.git

library
  exposed-modules:     Network.AWS.Loup
  other-modules:       Network.AWS.Loup.Act
                     , Network.AWS.Loup.Converge
                     , Network.AWS.Loup.Ctx
                     , Network.AWS.Loup.Decide
                     , Network.AWS.Loup.Prelude
                     , Network.AWS.Loup.Types
                     , Network.AWS.Loup.Types.Alias
                     , Network.AWS.Loup.Types.Ctx
                     , Network.AWS.Loup.Types.Product
                     , Network.AWS.Loup.Types.Sum
  default-language:    Haskell2010
  hs-source-dirs:      src
  ghc-options:         -Wall
  build-depends:       aeson
                     , amazonka
                     , amazonka-swf
                     , base >= 4.8 && < 5
                     , bytestring
                     , conduit
                     , lifted-async
                     , lifted-base
                     , preamble
                     , time
                     , turtle
                     , unordered-containers
                     , uuid
                     , yaml

executable loup-actor
  hs-source-dirs:      main
  main-is:             actor.hs
  ghc-options:         -threaded -rtsopts -with-rtsopts=-N -Wall
  build-depends:       base
                     , loup
                     , optparse-generic
  default-language:    Haskell2010

executable loup-decider
  hs-source-dirs:      main
  main-is:             decider.hs
  ghc-options:         -threaded -rtsopts -with-rtsopts=-N -Wall
  build-depends:       base
                     , loup
                     , optparse-generic
  default-language:    Haskell2010

executable loup-converger
  hs-source-dirs:      main
  main-is:             converger.hs
  ghc-options:         -threaded -rtsopts -with-rtsopts=-N -Wall
  build-depends:       base
                     , loup
                     , optparse-generic
  default-language:    Haskell2010

executable shake-loup
  main-is:             Shakefile.hs
  ghc-options:         -threaded -rtsopts -with-rtsopts=-N -Wall
  build-depends:       base >= 4.8 && < 5
                     , shakers
  default-language:    Haskell2010
