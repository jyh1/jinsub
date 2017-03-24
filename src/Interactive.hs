{-# LANGUAGE OverloadedStrings #-}
module Interactive where

import Control.Concurrent.Async
import Turtle
import System.Directory
import Filesystem.Path.CurrentOS
import Data.Text as T
import Prelude hiding (FilePath)
import Control.Exception.Base
import Control.Concurrent


interactName :: Text
interactName = "INTERACT"

interactMode :: Text -> IO ()
interactMode jobId = do
    name <- getFileName jobId
    let watcher = proc "tail" ["-F", pathToText name] stdin
    watchId <- async watcher
    echo "Ali?"
    sleep 10
    echo "Exit delay"
  where
    getFileName jobP = fmap (</> getName jobP) home
      where
        getId = fst . T.break (== '.')
        getName :: Text -> FilePath
        getName = fromText . T.append pre . getId
          where
            pre = T.append interactName ".o"


pathToText = fromEither . toText
  where
    fromEither (Left a) = a
    fromEither (Right a) = a
