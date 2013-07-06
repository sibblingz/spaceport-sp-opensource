{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module TH
  ( getCurrentTimeQ
  , runCmd
  ) where

#ifdef VERSION_process
import Control.Monad
import System.Exit
import System.IO
import System.Process

import qualified Control.Exception as Ex
#else
import A2J.Util.Error
#endif

import Data.Time
import Language.Haskell.TH

runCmd
  :: FilePath  -- ^ Command to execute.
  -> [String]  -- ^ Command arguments.
  -> String    -- ^ Default return value.
  -> (String -> String)  -- ^ Transformer.
  -> Q Exp
#ifdef VERSION_process
runCmd cmd args def transform
  = liftM (LitE . StringL) . runIO $ run `Ex.catch` onError
  where
    onError :: IOError -> IO String
    onError err = do
      hPutStrLn stderr $ "Warning: " ++ show err
      return def

    run = do
      (code, output, _) <- readProcessWithExitCode cmd args ""
      return $ case code of
        ExitSuccess -> transform output
        ExitFailure _ -> def

#else
runCmd cmd _ _ _ = $(internalErrorLate)
  ("Cannot run external command: " ++ cmd)
#endif

getCurrentTimeQ :: Q Exp
getCurrentTimeQ = do
  now <- runIO getCurrentTime
  let nowSerialized = show now
  [| read nowSerialized |]
