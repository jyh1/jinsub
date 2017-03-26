{-# LANGUAGE OverloadedStrings #-}
module Execution where

import Prelude hiding(FilePath)
import Filesystem.Path.CurrentOS
import qualified Data.Text as T
import Turtle
import Control.Monad(unless)
import System.Directory
import Data.Maybe(fromMaybe)
import Interactive(interactMode)


import OptionsParser
import Config

appName :: (IsString a) => a
appName = "jinsub"

addConfigExtension a = a <.> "jinsub"

execute :: Options -> IO ()
execute opts = sh (executeSh opts)
  where
    executeSh opts = do
      path <- getDataDirectory
      temp <- getTemplate opts path
      qid <- qsub opts temp
      when (interactive opts) (liftIO (interactMode qid))

qsub :: Options -> FilePath -> Shell Text
qsub opts pbs = do
  jid <- inproc "qsub" [pathToText pbs] stdin
  return (lineToText jid)


getTemplate :: Options -> FilePath -> Shell FilePath
getTemplate opts dataPath = do
    dir <- fmap pathToText pwd
    tempF <- getOutputFile opts dataPath -- temporary file to write
    inputTemp <- getTemplateFile opts dataPath -- loaded template file
    output tempF (generatePBS opts inputTemp)
    return tempF
  where

    getTemplateFile :: Options -> FilePath -> Shell FilePath
    getTemplateFile opts dataPath =
      let
        templateOption = fromString (template opts)
        tempP = addConfigExtension (dataPath </> templateOption)
        fileP = fmap fromString (fileTemplate opts)
        templateToRead = fromMaybe tempP fileP
      in
        do
          ifExist <- testfile templateToRead
          unless ifExist $ do
            printf ("Cannot find template file \""%fp%"\". Exiting.") templateToRead
            exit (ExitFailure (-1))
          return templateToRead

    getOutputFile :: Options -> FilePath -> Shell FilePath
    getOutputFile opts path = do
      using (mktempfile path appName)

    generatePBS :: Options -> FilePath -> Shell Line
    generatePBS opts temp = do
      dir <- pwd
      let
        replacement = ("CMD", getCmd opts)
                        : ("WorkingDirectory", pathToText dir)
                        : variableTerm opts
        pat = foldr1 (<|>) (map getPat replacement)

      sed pat (input temp)
      where
        getPat :: (Text, Text) -> Pattern Text
        getPat (a, b) = text (getPlaceHolder a) *> return b
        getCmd opts = T.pack (unwords (command opts))
        getPlaceHolder :: Text -> Text
        getPlaceHolder = format ("#("%s%")")

pathToText = format fp


getDataDirectory :: Shell FilePath
getDataDirectory = do
  path <- fmap fromString (liftIO (getAppUserDataDirectory appName))
  exist <- testdir path
  unless exist $ do
    mkdir path
    liftIO (writeTextFile (addConfigExtension (path </> "default")) defaultPBS)
  return path
