{-# LANGUAGE OverloadedStrings #-}
module Execution where

import Prelude hiding(FilePath)
import Filesystem.Path.CurrentOS
import qualified Data.Text as T
import Turtle
import Control.Monad
import System.Directory


import OptionsParser
import Config

appName = "jinsub"
addConfigExtension a = a <.> "jinsub"

execute :: Options -> IO ()
execute opts = sh (executeSh opts)
  where
    executeSh opts = do
      path <- getDataDirectory
      temp <- getTemplate opts path
      qsub opts temp

qsub :: Options -> FilePath -> Shell ()
qsub opts pbs = do
  proc "qsub" [pathToText pbs] stdin
  return ()


getTemplate :: Options -> FilePath -> Shell FilePath
getTemplate opts dataPath = do
    dir <- fmap pathToText pwd
    let vs = ("CurrentDirectory", dir) : getArgs opts
    tempF <- getOutputFile opts dataPath
    inputTemp <- getTemplateFile opts dataPath
    output tempF (generatePBS inputTemp vs)
    return tempF
  where

    getTemplateFile :: Options -> FilePath -> Shell FilePath
    getTemplateFile opts dataPath =
      let
        givenP = fromString (template opts)
        tempP = addConfigExtension (dataPath </> givenP)
      in
        do
          ifExist <- testfile givenP
          if ifExist then return givenP else return tempP


    getArgs :: Options -> [(Text, Text)]
    getArgs opts =
      [("CMD", T.pack (unwords (command opts)))]

    getOutputFile :: Options -> FilePath -> Shell FilePath
    getOutputFile opts path = do
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

pathToText = fromEither . toText
  where
    fromEither (Left a) = a
    fromEither (Right a) = a


getDataDirectory :: Shell FilePath
getDataDirectory = do
  path <- fmap fromString (liftIO (getAppUserDataDirectory appName))
  exist <- testdir path
  unless exist $ do
    mkdir path
    liftIO (writeTextFile (addConfigExtension (path </> "default")) defaultPBS)
  return path
