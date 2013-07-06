{-# LANGUAGE DeriveDataTypeable #-}

module System.Process.Handle
  ( RunningProc
  , ProcessException(..)
  , readProcessHandles
  , runProcessHandles
  , runProcessHandles'
  , runProcessHandles_
  , askProcessOutput
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Exception hiding (handle)
import Control.Monad
import Data.Typeable
import System.Exit
import System.IO hiding (stdin, stdout, stderr)
import System.Process

import qualified Data.ByteString as BS

import Development.Spaceport.Util

-- | Information about a running process.
type RunningProc = (Handle, Handle, Handle, ProcessHandle)

data ProcessException = ProcessException
  { exceptionMessage :: String
  , exceptionExitCode :: ExitCode
  , exceptionStdout :: BS.ByteString
  , exceptionStderr :: BS.ByteString
  } deriving (Typeable)

instance Show ProcessException where
  show (ProcessException msg _ stdout stderr) = concat
    [ msg, "\n"
    , bytestringToStringUTF8 stdout
    , bytestringToStringUTF8 stderr
    ]

instance Exception ProcessException where

readProcessHandles
  :: RunningProc
  -> IO (ExitCode, BS.ByteString, BS.ByteString)
readProcessHandles (stdin, stdout, stderr, pid) = do
  hClose stdin
  out <- hGetContentsLate stdout
  err <- hGetContentsLate stderr
  exitCode <- waitForProcess pid
  (,,) exitCode <$> takeMVar out <*> takeMVar err

runProcessHandles
  :: String            -- ^ Failure message.
  -> RunningProc
  -> IO BS.ByteString  -- ^ Standard output.
runProcessHandles failureMessage handles
  = fst <$> runProcessHandles' failureMessage handles

runProcessHandles'
  :: String  -- ^ Failure message.
  -> RunningProc
  -> IO (BS.ByteString, BS.ByteString)
  -- ^ Standard output, error.
runProcessHandles' failureMessage handles
  = readProcessHandles handles
    >>= \ (exitCode, stdout, stderr) -> do
      checkExitCode failureMessage exitCode stdout stderr
      return (stdout, stderr)

runProcessHandles_
  :: String            -- ^ Failure message.
  -> RunningProc
  -> IO ()
runProcessHandles_ failureMessage handles
  = void $ runProcessHandles failureMessage handles

-- | Gets stderr on failure, stdout on success.
askProcessOutput
  :: RunningProc
  -> IO (Either BS.ByteString BS.ByteString)
askProcessOutput handles = do
  (code, stdout, stderr) <- readProcessHandles handles
  return $ case code of
    ExitFailure _ -> Left stderr
    ExitSuccess -> Right stdout

checkExitCode
  :: String         -- ^ Failure message.
  -> ExitCode
  -> BS.ByteString  -- ^ Standard output.
  -> BS.ByteString  -- ^ Standard error.
  -> IO ()
checkExitCode failureMessage exitCode stdout stderr
  = case exitCode of
    ExitSuccess -> return ()
    ExitFailure _ -> throwIO ProcessException
      { exceptionMessage = failureMessage
      , exceptionExitCode = exitCode
      , exceptionStdout = stdout
      , exceptionStderr = stderr
      }

hGetContentsLate :: Handle -> IO (MVar BS.ByteString)
hGetContentsLate handle = do
  var <- newEmptyMVar
  void . forkIO $ putMVar var =<< BS.hGetContents handle
  return var
