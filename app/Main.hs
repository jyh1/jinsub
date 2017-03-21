module Main where

import OptionsParser

main :: IO ()
main = do
  opt <- getOptions
  print opt
