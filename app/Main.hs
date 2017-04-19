{-# LANGUAGE OverloadedStrings #-}
module Main where

import OptionsParser
import Execution
import Interactive
import Edit

main :: IO ()
main = do
  opt <- getOptions
  case opt of
    Main o -> execute o
    Subedit e -> executeEdit e
