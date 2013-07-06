{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main (main) where

import Control.Applicative
import Control.Exception as Ex
import Data.List
import Data.Monoid
import Data.Typeable
import Network.MDNS
import Network.MDNS.Load.Common
import Options.Applicative
import Options.Applicative.Types
import System.Environment
import System.Exit
import System.IO
import System.Java.Locate

#ifdef AVAHI
import System.Posix.Env
#endif

import BashCompletion
import Command

failWithParserFailure :: ParserFailure -> IO ()
failWithParserFailure err = do
  progName <- getProgName
  msg <- errMessage err progName
  case code of
    ExitSuccess -> putStr msg
    _           -> hPutStr stderr msg
  exitWith code
  where code = errExitCode err

filterMessage :: String -> String
filterMessage
  = unlines
  . filter (not . isHiddenCommand)
  . lines
  where
    isHiddenCommand line
      = "  build " `isPrefixOf` line
      || "  serve " `isPrefixOf` line

parseArgs :: ParserPrefs -> ParserInfo a -> [String] -> IO a
parseArgs pprefs pinfo args = case execParserPure pprefs pinfo args of
  Right x -> return x
  Left err -> do
    progName <- getProgName
    let code = errExitCode err
    msg <- filterMessage <$> errMessage err progName
    case code of
      ExitSuccess -> putStr msg
      _ -> hPutStr stderr msg
    exitWith code

main :: IO ()
main = do
#ifdef AVAHI
  -- Tell Avahi not to warn about using the DNS-SD
  -- compatibility layer on Linux.
  setEnv "AVAHI_COMPAT_NOWARN" "1" False
#endif

  -- Tools may want to capture our output (e.g. `sp push`
  -- logs).  This makes it easy for them.
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  -- This is awkward.  There has to be a better way...
  mCompletion <- execParserPure parserPrefs
    (info (bashCompletionParser parser parserPrefs) desc)
    <$> getArgs
  case mCompletion of
    Left _ -> return ()
    Right err -> failWithParserFailure err

  catches go
    [ Handler mDNSMissing
    , Handler mDNSFailed
    , Handler jvmNotFound
    ]

  where
    go = execCommand
      =<< parseArgs parserPrefs (info parser desc)
      =<< getArgs

    desc = mconcat
      [ fullDesc
      , progDesc "Builds and deploys an ActionScript 3 project using Spaceport."
      ]

    parserPrefs = ParserPrefs
      { prefMultiSuffix = ""
      , prefDisambiguate = False
      , prefShowHelpOnError = True
      , prefBacktrack = True
      }

    parser = helper <*> parseCommand

data MDNSError = MDNSError SomeException
  deriving (Typeable)

instance Show MDNSError where
  show (MDNSError err)
    = "The Bonjour service is having problems."
    ++ "\nError is: " ++ show err
#ifdef WINDOWS_TOOLS
    ++ "\nPlease install iTunes: https://www.apple.com/itunes/download/"
#endif
#ifdef OSX_TOOLS
    ++ "\nPlease report this issue to the Spaceport team, because"
    ++ " it should come with your operating system."
#endif
#ifdef LINUX_TOOLS
   ++ "\nPlease install Avahi and the DNS-SD compatibility layer."
#endif

instance Exception MDNSError

data JREError = JREError JVMNotFound
  deriving (Typeable)

instance Show JREError where
  show (JREError JVMNotFound)
    = "The Java Runtime Environment (JRE) could not be found."
#ifdef WINDOWS_TOOLS
    ++ "\nPlease install Java SE 7: http://www.oracle.com/technetwork/java/javase/downloads/index.html"
#else
    ++ "\nPlease ensure the 'java' executable is in your PATH."
#endif

instance Exception JREError

mDNSMissing :: MDNSAPILoadError -> IO ()
mDNSMissing = throwIO . MDNSError . toException

mDNSFailed :: ServiceError -> IO ()
mDNSFailed = throwIO . MDNSError . toException

jvmNotFound :: JVMNotFound -> IO ()
jvmNotFound = throwIO . JREError
