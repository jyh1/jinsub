{-# LANGUAGE OverloadedStrings #-}
module Execution where

import Prelude hiding(FilePath)
import Filesystem.Path.CurrentOS
import qualified Data.Text as T
import Turtle
import Control.Monad(unless)
import Data.Maybe(fromMaybe, isNothing)
import Interactive(interactMode, interactName, jobEndSignal)


import OptionsParser
import Config


execute :: Options -> IO ()
execute opts = sh (executeSh opts)
  where
    executeSh opts = do
      path <- getDataDirectory
      temp <- getTemplate opts path
      saveFile opts temp
      if dryRun opts then
        when (isNothing (saveAs opts)) $ stdout (input temp)
      else
        submit opts temp

    submit opts temp =
      let
        qsubOpt = if interactive opts then ["-N", interactName, "-k", "oe"] else []
        addEOF = Turtle.append temp (select ["echo 1>&2", unsafeTextToLine
                                                            (format ("echo "%s% " 1>&2") jobEndSignal)])
      in do
        when (interactive opts) addEOF
        qid <- qsub opts qsubOpt temp
        when (interactive opts) $
          liftIO (interactMode qid)

qsub :: Options -> [Text] -> FilePath -> Shell Text
qsub opts qsubOpt pbs = do
  jid <- inproc "qsub" (qsubOpt ++ [pathToText pbs]) stdin
  err jid
  return (lineToText jid)

saveFile :: Options -> FilePath -> Shell ()
saveFile opts temp = mapM_ (cp temp . fromText . T.pack) (saveAs opts)

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
  exist <- testfile defaultTemplate
  unless exist $
    liftIO (writeTextFile defaultTemplate defaultPBS)
  return homePath
