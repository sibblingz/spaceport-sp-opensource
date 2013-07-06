module Control.Concurrent.SpExtras
  ( Microseconds
  , seconds

  , forkTimeout
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async

import Development.Spaceport.Util

type Microseconds = Int

seconds :: Microseconds
seconds = 1000 * 1000

forkTimeout :: Microseconds -> IO a -> IO (Maybe a)
forkTimeout timeout m = eitherToMaybe
  <$> race (threadDelay timeout) m
