{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module Compdef where

import Antigen
import Shelly (shelly)

bundles =
  [ developFromFileSystem "/tmp/antigen-hs/tests/repos/compdef"
  ]

config = AntigenConfiguration bundles

main :: IO ()
main = shelly $ antigen config

