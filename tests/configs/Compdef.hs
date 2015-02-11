{-# LANGUAGE OverloadedStrings #-}
module Compdef where

import Antigen
import Shelly (shelly)

bundles =
  [ (developFromFileSystem "/tmp/antigen-hs/tests/repos/compdef") {
      sourcingStrategy = antigenSourcingStrategy,
      fpathLocations = ["etc"]
    }
  ]

config = AntigenConfiguration bundles

main :: IO ()
main = shelly $ antigen config
