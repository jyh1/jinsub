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

data Resources = Resources {
    oPath :: FilePath
  , ePath :: FilePath
  , oHandle :: Handle
  , oProcess :: ProcessHandle
  , eProcess :: ProcessHandle
  , jobId :: Text
}

interactName :: Text
interactName = "INTERACT"

jobEndSignal :: Text
jobEndSignal = "Jinsub_Interactive_EOF"

interactMode :: Text -> IO ()
interactMode jobId = do
  (name, errName) <- getFileName jobId
  let
    (sname, serrName) = both (T.unpack . format fp) (name, errName)
    watcher = createProcess_ "watcher" (tailProcess sname CreatePipe)
    errWatcher = createProcess_ "watcher_stderr" (tailProcess serrName (UseHandle stderr))
    qdel = createProcess_ "qdel" (generateProcess ("qdel "++T.unpack jobId) NoStream)
  finally
    (bracket
      (runWatcher watcher)
      (terminateProcess . snd)
      (\(outH, wHandle) ->
        bracket
          (runErrWatcher errWatcher)
          terminateProcess
          (\_ -> iter outH)
      )
    )
    (qdelAndDelete qdel name errName)
  where
    runWatcher watcher = do
      (_, Just outH, _, wHandle) <- watcher
      return (outH, wHandle)
    runErrWatcher errWatcher = do
      (_, _, _, weHandle) <- errWatcher
      return weHandle
    qdelAndDelete qdelP name errName = do
      runAndWait qdelP
      remove name
      remove errName

    remove :: FilePath -> IO ()
    remove name =
      let sn = T.unpack (format fp name) in
        runAndWait (createProcess_ "remove_forcibly" (generateProcess ("rm -f " ++ sn) NoStream))

    runAndWait p = do
      (_, _, _, h) <- p
      _ <- waitForProcess h
      return ()

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


iter :: Handle -> IO ()
iter outH = do
  l <- hGetLine outH
  unless (l == jobEndSignal) $
    do
      putStrLn l
      iter outH


pathToText = format fp

tailProcess :: String -> StdStream -> CreateProcess
tailProcess str = generateProcess ("tail -n +1 --follow=name --retry " ++ str)


generateProcess :: String -> StdStream -> CreateProcess
generateProcess str stream = CreateProcess { cmdspec = ShellCommand str,
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
