{-# LANGUAGE OverloadedStrings #-}
module Interactive where

-- import Control.Concurrent.Async
import Turtle
import System.Process
import Data.Text as T
import Prelude hiding (FilePath)
import Control.Exception.Base
import Control.Concurrent
import Control.Exception

interactName :: Text
interactName = "INTERACT"

jobEndSignal :: Line
jobEndSignal = "Jinsub_Interactive_EOF"

interactMode :: Text -> IO ()
interactMode jobId =
    catch asyncMonitor (onUserInterupt undefined)
  where
    asyncMonitor = sh $ do
      name <- getFileName jobId
      let
        cmd = format ("tail -n +1 --follow=name --retry " %fp % "2> /dev/null") name
        watcher = inshell cmd stdin
      res  <- watcher
      echo res
    getFileName jobP = fmap (</> getName jobP) home
      where
        getId = fst . T.break (== '.')
        getName :: Text -> FilePath
        getName = fromText . T.append pre . getId
          where
            pre = T.append interactName ".o"


onUserInterupt :: FilePath ->  AsyncException -> IO ()
onUserInterupt _ e = do
  putStrLn "Exiting!"
  print e



pathToText = fromEither . toText
  where
    fromEither (Left a) = a
    fromEither (Right a) = a
