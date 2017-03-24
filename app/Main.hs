{-# LANGUAGE OverloadedStrings #-}
module Main where

import OptionsParser
import Execution
import Interactive

main :: IO ()
main = do
  -- opt <- getOptions
  -- execute opt
  interactMode "234.bio"
