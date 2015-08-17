{-# LANGUAGE OverloadedStrings #-}
module Compdef where

import Antigen

bundles =
  [ (local "/tmp/antigen-hs/tests/repos/compdef") {
      sourcingStrategy = antigenSourcingStrategy,
      fpathLocations = ["etc"]
    }
  ]

config = defaultConfig { plugins = bundles }

main :: IO ()
main = antigen config
