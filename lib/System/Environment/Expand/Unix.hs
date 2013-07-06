module System.Environment.Expand.Unix
  ( expandPath
  , tildeExpand
  ) where

import System.Posix.User

import qualified Control.Exception as Ex

expandPath :: FilePath -> IO FilePath
expandPath = tildeExpand

tildeExpand :: FilePath -> IO FilePath
tildeExpand input@('~' : remainder) = Ex.catch expand ignore
  where
    ignore :: IOError -> IO FilePath
    ignore = const (return input)
    (username, rest) = span (/= '/') remainder
    expand = do
      user <- if null username then getEffectiveUserName else return username
      entry <- getUserEntryForName user
      return $ homeDirectory entry ++ rest
tildeExpand x = return x
