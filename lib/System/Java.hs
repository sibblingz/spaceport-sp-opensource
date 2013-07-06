module System.Java
  ( JVMOption
  , ClassPath
  , JavaClass

  , JavaPath
  , Environment

  , runJVM
  ) where

import Data.List
import System.FilePath
import System.IO hiding (stdin, stdout, stderr)
import System.Process

type JVMOption = [String]
type ClassPath = String
type JavaClass = String

type JavaPath = String
type Environment = [(String, String)]

runJVM
  :: JavaPath
  -> [JVMOption]
  -> [ClassPath]
  -> Maybe FilePath  -- ^ Current working directory.
  -> Maybe Environment
  -> JavaClass       -- ^ Entry point class.
  -> [String]        -- ^ Program arguments.
  -> IO (Handle, Handle, Handle, ProcessHandle)
runJVM
    javaPath jvmOptions classPaths mCwd mEnv
    javaClass programArgs
  = runInteractiveProcess javaPath args mCwd mEnv
  where
    classPath = intercalate [searchPathSeparator] classPaths
    args = concat
      [ concat jvmOptions
      , ["-cp", classPath]
      , [javaClass]
      , programArgs
      ]
