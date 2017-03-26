{-# LANGUAGE OverloadedStrings #-}
module Interactive where

-- import Control.Concurrent.Async
import Turtle hiding (env, stderr, stdout)
import System.Process
import System.IO(stdout, stderr)
import Data.Text as T
import Prelude hiding (FilePath, putStrLn)
import Control.Exception.Base
import Control.Concurrent
import Control.Exception
import Data.Text.IO
import Data.Tuple.Extra ((&&&), both)


interactName :: Text
interactName = "INTERACT"

jobEndSignal :: Text
jobEndSignal = "Jinsub_Interactive_EOF"

interactMode :: Text -> IO ()
interactMode jobId =
    catch monitor (onUserInterupt undefined)
  where
    monitor = do
      (name, errName) <- getFileName jobId
      let
        (sname, serrName) = both (T.unpack . format fp) (name, errName)
        watcher = createProcess_ "watcher" (tailProcess sname CreatePipe)
        errWatcher = createProcess_ "watcher_stderr" (tailProcess serrName (UseHandle stderr))
      (_, Just outH, _, wHandle)  <- watcher
      (_, _, _, weHandle) <- errWatcher
      iter outH
      terminateProcess wHandle
      terminateProcess weHandle
    iter :: Handle -> IO ()
    iter outH = do
      l <- hGetLine outH
      unless (l == jobEndSignal) $
        do
          putStrLn l
          iter outH
    getFileName jobP = do
      h <- home
      return $ both (h </>) (getName jobP)
      where
        getId = fst . T.break (== '.')
        getName :: Text -> (FilePath, FilePath)
        getName = (getPath pre &&& getPath errPre) . getId
          where
            pre = T.append interactName ".o"
            errPre = T.append interactName ".e"
            getPath p = fromText . T.append p


onUserInterupt :: FilePath ->  AsyncException -> IO ()
onUserInterupt _ e = do
  putStrLn "Exiting!"
  print e

finalize :: ((ProcessHandle, FilePath), (ProcessHandle, FilePath)) -> IO ()
finalize = undefined

pathToText = fromEither . toText
  where
    fromEither (Left a) = a
    fromEither (Right a) = a


tailProcess :: String -> StdStream -> CreateProcess
tailProcess str stream = CreateProcess { cmdspec = ShellCommand ("tail -n +1 --follow=name --retry " ++ str),
                                  cwd = Nothing,
                                  env = Nothing,
                                  std_in = NoStream,
                                  std_out = stream,
                                  std_err = NoStream,
                                  close_fds = False,
                                  create_group = False,
                                  delegate_ctlc = False,
                                  detach_console = False,
                                  create_new_console = False,
                                  new_session = False,
                                  child_group = Nothing,
                                  child_user = Nothing }
