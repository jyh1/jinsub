{-# LANGUAGE OverloadedStrings #-}
module Execution where

import Prelude hiding(FilePath)
import Filesystem.Path.CurrentOS
import qualified Data.Text as T
import Turtle

import OptionsParser



execute :: Options -> IO ()
execute opts = do
    dir <- pwd
    stdout $ generatePBS (getTemplateFile opts) [("CurrentDirectory", repr dir)]
  where
    getTemplateFile :: Options -> FilePath
    getTemplateFile opts = fromText (T.pack $ template opts)


generatePBS :: FilePath -> [(Text, Text)] -> Shell Line
generatePBS temp vars = do
  let varDefs = map (uncurry getDef) vars
      pat = defPlaceHolder *> return (T.unlines varDefs)
  sed pat (input temp)
  where
    getDef :: Text -> Text -> Text
    getDef = format (s % "=\"" %s% "\"")
    defPlaceHolder = "#DEFS"
