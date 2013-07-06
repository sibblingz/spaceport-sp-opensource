module Development.ActionScript3.Project
  ( Project(..)
  ) where

data Project = Project
  { projectSourceDirs :: [FilePath]
  , projectLibraryDirs :: [FilePath]
  , projectLibraryFiles :: [FilePath]
  , projectAS3EntryPoint :: FilePath
  } deriving (Eq, Show)
