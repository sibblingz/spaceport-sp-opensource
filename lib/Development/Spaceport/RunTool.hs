module Development.Spaceport.RunTool
  ( runHidden
  , runHidden_
  , runConvert
  , runLive
  ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import System.IO
import System.Process
import System.Process.Handle

import qualified Data.ByteString as BS

-- | Runs a process, showing output only if the process
-- exits with a non-zero exit code.  Intended for internal
-- or background tasks.
runHidden
  :: (MonadIO m)
  => String
  -> m RunningProc
  -> m (BS.ByteString, BS.ByteString)
runHidden message m
  = m >>= liftIO . runProcessHandles' failureMessage
  where failureMessage = "Failed to " ++ message

-- | Like 'runHidden', but without a result.
runHidden_
  :: (MonadIO m)
  => String
  -> m RunningProc
  -> m ()
runHidden_ message m
  = m >>= liftIO . runProcessHandles_ failureMessage
  where failureMessage = "Failed to " ++ message

-- | Runs a process, buffering its output until the process
-- completes.  Indented for tasks dealing with user files.
runConvert
  :: (MonadIO m)
  => String
  -> m RunningProc
  -> m ()
runConvert message m = do
  (out, err) <- m >>= liftIO . runProcessHandles' failureMessage
  liftIO $ do
    BS.hPut stdout out
    BS.hPut stderr err
  where failureMessage = "Failed to convert " ++ message

hCopy :: Handle -> Handle -> IO ()
hCopy from to = forever $ BS.hGetSome from nr >>= BS.hPut to
  where nr = 16

runLive
  :: (MonadIO m)
  => m RunningProc
  -> m ProcessHandle
runLive m = do
  (myStdin, myStdout, myStderr, pid) <- m
  liftIO $ do
    hClose myStdin
    void . forkIO $ hCopy myStdout stdout
    void . forkIO $ hCopy myStderr stderr
  return pid
