module Edit where

import System.Process
import Data.Text as T
import Filesystem.Path.CurrentOS as P
import OptionsParser
import Data.Maybe(isNothing)
import Turtle


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
  let fpath = P.fromText (T.pack f) in
    if checkTemplate f
      then
        runEditor (T.unpack (format fp (getTemplatePath fpath)))
      else runEditor f
