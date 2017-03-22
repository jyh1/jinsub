{-# LANGUAGE OverloadedStrings #-}
module Execution where

import Prelude hiding(FilePath)
import Filesystem.Path.CurrentOS
import qualified Data.Text as T
import Turtle


import OptionsParser
import System.Directory


execute :: Options -> IO ()
execute opts = do
  undefined

getTemplate :: Options -> Shell FilePath
getTemplate opts = do
    dir <- fmap (fromEither . toText) pwd
    let vs = ("CurrentDirectory", dir) : getArgs opts
    tempF <- getOutputFile opts
    output tempF (generatePBS (getTemplateFile opts) vs)
    return tempF
  where
    fromEither (Left a) = a
    fromEither (Right a) = a

    getTemplateFile :: Options -> FilePath
    getTemplateFile opts = fromText (T.pack $ template opts)

    getArgs :: Options -> [(Text, Text)]
    getArgs opts =
      [("CMD", T.pack (unwords (command opts)))]

    getOutputFile :: Options -> Shell FilePath
    getOutputFile opts = do
      path <- fmap fromString (liftIO (getAppUserDataDirectory ""))
      using (mktempfile path "jinsub")

    generatePBS :: FilePath -> [(Text, Text)] -> Shell Line
    generatePBS temp vars = do
      let varDefs = map (uncurry getDef) vars
          pat = defPlaceHolder *> return (T.unlines varDefs)
      sed pat (input temp)
      where
        getDef :: Text -> Text -> Text
        getDef = format (s % "=\"" %s% "\"")
        defPlaceHolder = "#DEFS"
