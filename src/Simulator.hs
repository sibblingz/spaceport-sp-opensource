{-# LANGUAGE CPP #-}

module Simulator
  ( launchSimulator
  ) where

import Control.Exception as Ex
import Control.Monad
import System.Process hiding (cwd)

import qualified Network as Net

#ifdef OSX_TOOLS
import qualified System.Process.Internals as Process
#endif

import Development.Spaceport.Support
import Development.Spaceport.Tools.Simulator

launchSimulator
  :: Tools IO
  -> Net.PortNumber
  -> IO ()
launchSimulator tools serverPort = do
  putStrLn $ "Launching " ++ url ++ " on simulator..."
  void $ Ex.bracket runProc terminateProcess waitForProcess

  where
    url = "http://localhost:" ++ show serverPort ++ "/manifest.xml"

    runProc = do
      -- See note [Run simulator].
      (_, _, _, pid) <- simulator tools Simulator
        { launchUrl = Just url }

#ifdef HLINT
      -- hlint complains because of:
      --   do { x <- y ; return x }
      -- This works around the hlint error.
      _ <- return ()
#endif

#ifdef OSX_TOOLS
      makePIDFrontmost pid
#endif

      return pid

#ifdef OSX_TOOLS
makePIDFrontmost :: ProcessHandle -> IO ()
makePIDFrontmost pid = do
  script <- makePIDFrontmostScript pid
  void $ readProcess "osascript" [] script

type AppleScript = String

makePIDFrontmostScript :: ProcessHandle -> IO AppleScript
makePIDFrontmostScript pid = do
  pidNumber <- Process.withProcessHandle pid getPIDNumber

  return $ unlines
    [ "tell application \"System Events\""
    , "  set theprocs to every process whose unix id is " ++ show pidNumber
    , "  repeat with proc in theprocs"
    , "    set the frontmost of proc to true"
    , "  end repeat"
    , "end tell"
    ]

  where
    getPIDNumber pidHandle = return . (,) pidHandle
      $ case pidHandle of
        Process.OpenHandle p -> p
        Process.ClosedHandle _
          -> error "Could not get open PID handle of process."
#endif
