{-# LANGUAGE OverloadedStrings #-}
module Basic where

import Antigen
import Shelly (shelly)

bundles =
  [ bundle "Tarrasch/zsh-bd"
  , bundle "Tarrasch/zsh-mcd"
  ]

config = AntigenConfiguration bundles

main :: IO ()
main = shelly $ antigen config
