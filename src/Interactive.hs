{-# LANGUAGE OverloadedStrings #-}
module Interactive where

-- import Control.Concurrent.Async
import Turtle hiding (env)
import System.Process
import Data.Text as T
import Prelude hiding (FilePath, putStrLn)
import Control.Exception.Base
import Control.Concurrent
import Control.Exception
import Data.Text.IO

interactName :: Text
interactName = "INTERACT"

jobEndSignal :: Text
jobEndSignal = "Jinsub_Interactive_EOF"

interactMode :: Text -> IO ()
interactMode jobId =
    catch monitor (onUserInterupt undefined)
  where
    monitor = do
      name <- getFileName jobId
      let
        watcher = createProcess_ "watcher" (tailProcess (T.unpack (format fp name)))
      (_, Just outH, _, wHandle)  <- watcher
      iter outH
      terminateProcess wHandle
    iter :: Handle -> IO ()
    iter outH = do
      l <- hGetLine outH
      unless (l == jobEndSignal) $
        do
          putStrLn l
          iter outH
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

tailProcess :: String -> CreateProcess
tailProcess str = CreateProcess { cmdspec = ShellCommand ("tail -n +1 --follow=name --retry " ++ str),
                                  cwd = Nothing,
                                  env = Nothing,
                                  std_in = NoStream,
                                  std_out = CreatePipe,
                                  std_err = NoStream,
                                  close_fds = False,
                                  create_group = False,
                                  delegate_ctlc = False,
                                  detach_console = False,
                                  create_new_console = False,
                                  new_session = False,
                                  child_group = Nothing,
                                  child_user = Nothing }
