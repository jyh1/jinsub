{-# LANGUAGE OverloadedStrings #-}
module Execution where

import Prelude hiding(FilePath)
import Filesystem.Path.CurrentOS
import qualified Data.Text as T
import Turtle

import OptionsParser



execute :: Options -> IO ()
execute opts = do
    dir <- fmap (fromEither . toText) pwd
    let vs = ("CurrentDirectory", dir) : getArgs opts
    stdout $ generatePBS (getTemplateFile opts) vs
  where
    fromEither (Left a) = a
    fromEither (Right a) = a

    getTemplateFile :: Options -> FilePath
    getTemplateFile opts = fromText (T.pack $ template opts)

    getArgs :: Options -> [(Text, Text)]
    getArgs opts =
      [("CMD", T.pack (unwords (command opts)))]

-- getTemplaet

generatePBS :: FilePath -> [(Text, Text)] -> Shell Line
generatePBS temp vars = do
  let varDefs = map (uncurry getDef) vars
      pat = defPlaceHolder *> return (T.unlines varDefs)
  sed pat (input temp)
  where
    getDef :: Text -> Text -> Text
    getDef = format (s % "=\"" %s% "\"")
    defPlaceHolder = "#DEFS"
