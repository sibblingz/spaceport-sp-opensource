{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module System.IO.BlockFD
  ( blockRead
  ) where

#ifdef USE_CUSTOM_THREAD_WAIT
import Control.Concurrent.Async (asyncBound, wait)
import Foreign.C.Types
#else
import Control.Concurrent (threadWaitRead)
#endif
import System.Posix.Types

-- | Wait until data is available on the given file
-- descriptor.
blockRead :: Fd -> IO ()

#ifdef USE_CUSTOM_THREAD_WAIT
-- int thread_wait_read(int fd);
foreign import ccall "blockfd.c thread_wait_read"
  foreign_thread_wait_read :: CInt -> IO CInt

-- Only tested with threaded runtime!
blockRead (Fd fd) = do
  -- Block in a separate OS thread so Haskell code can still
  -- live on.
  _errCode <- wait =<< asyncBound (foreign_thread_wait_read fd)
  -- FIXME We should do something with _errCode.
  return ()

#else
-- Does *not* work on Windows!
blockRead = threadWaitRead
#endif
