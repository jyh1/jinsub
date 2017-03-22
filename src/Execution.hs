{-# LANGUAGE OverloadedStrings #-}
module Execution where

import Prelude hiding(FilePath)
import Filesystem.Path.CurrentOS
import qualified Data.Text as T
import Turtle
import Control.Monad(unless)
import System.Directory
import Data.Maybe(fromMaybe)


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
      qsub opts temp

qsub :: Options -> FilePath -> Shell ()
qsub opts pbs = do
  proc "qsub" [pathToText pbs] stdin
  return ()


getTemplate :: Options -> FilePath -> Shell FilePath
getTemplate opts dataPath = do
    dir <- fmap pathToText pwd
    let vs = ("CurrentDirectory", dir) : getArgs opts
    tempF <- getOutputFile opts dataPath -- temporary file to write
    inputTemp <- getTemplateFile opts dataPath -- template file
    output tempF (generatePBS opts inputTemp vs)
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


    getArgs :: Options -> [(Text, Text)]
    getArgs opts =
      -- [("CMD", T.pack (unwords (command opts)))]
      []

    getOutputFile :: Options -> FilePath -> Shell FilePath
    getOutputFile opts path = do
      using (mktempfile path appName)

    generatePBS :: Options -> FilePath -> [(Text, Text)] -> Shell Line
    generatePBS opts temp vars = do
      let varDefs = map (uncurry getDef) vars
          pat = (defPlaceHolder *> return (T.unlines varDefs))
                <|> (cmdPlaceHolder *> return (getCmd opts))
      sed pat (input temp)
      where
        getDef :: Text -> Text -> Text
        getDef a b = format (s % "=\"" %s% "\"") (T.strip a) (T.strip b)
        getCmd opts = T.pack (unwords (command opts))
        defPlaceHolder = "#DEFS"
        cmdPlaceHolder = "#CMD"

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
