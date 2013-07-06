module System.Process.Runner
  ( RunningProc
  , Runner

  , ExeRunner
  ) where

import System.Process.Handle

-- | An abstract type for running a process under the given
-- monad with the given parameter type.
type Runner m opts = opts -> m RunningProc

type ExeRunner m = Runner m [String]
