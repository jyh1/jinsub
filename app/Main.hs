module Main where

import OptionsParser
import Execution

main :: IO ()
main = do
  opt <- getOptions
  execute opt
