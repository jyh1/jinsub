module Edit where

import System.Process
import Data.Text as T
import Filesystem.Path.CurrentOS as P
import OptionsParser
import Data.Maybe(isNothing)
import Turtle
import Config


editorName = "vi"

checkTemplate :: String -> Bool
checkTemplate f =
  notElem '.' f && notElem '/' f

runEditor :: String -> IO ()
runEditor f = do
  prc <- spawnProcess editorName [f]
  waitForProcess prc
  return ()

executeEdit :: EditCommand -> IO ()
executeEdit (EditCommand f) =
  let fpath = fromString f
      template = T.unpack (format fp (getTemplatePath fpath))
      target = if checkTemplate f then template else f
      targetP = fromString target in
    do
      exist <- testfile targetP
      unless exist $
        writeTextFile targetP defaultPBS
      runEditor target
