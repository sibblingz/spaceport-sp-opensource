{-# LANGUAGE CPP #-}

module System.Environment.Expand
  ( expandPath
  ) where

#ifdef EXPAND_UNIX
import System.Environment.Expand.Unix
#elif EXPAND_WINDOWS
import System.Environment.Expand.Windows
#else
import System.Environment.Expand.Fallback
#endif
  (expandPath)
