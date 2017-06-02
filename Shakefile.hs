#!/usr/bin/env stack
{- stack
    runghc
    --package shakers
 -}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Shake makefile for project.
--
import Development.Shakers

-- | Main entry point.
--
main :: IO ()
main = shakeMain $ do
  let pats =
        [ "stack.yaml"
        , "Shakefile.hs"
        , "main//*.hs"
        , "src//*.hs"
        ]

  -- | Haskell rules.
  --
  hsRules "."

  -- | Cabal rules.
  --
  cabalRules "." "loup.cabal"

  -- | Stack rules.
  --
  stackRules "." pats

  -- | sanity
  --
  fake "." pats "sanity" $ const $
    need [ "build-error", "lint" ]

  -- | Default things to run.
  --
  want [ "sanity", "format" ]
