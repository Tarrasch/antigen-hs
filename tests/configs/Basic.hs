{-# LANGUAGE OverloadedStrings #-}
module Basic where

import Antigen

bundles =
  [ bundle "Tarrasch/zsh-bd"
  , bundle "Tarrasch/zsh-mcd"
  ]

config = defaultConfig { plugins = bundles }

main :: IO ()
main = antigen config
