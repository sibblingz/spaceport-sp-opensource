{-# LANGUAGE RecordWildCards #-}

module Development.Spaceport.Tools.Aapt
  ( Aapt(..)
  , runAaptAction
  ) where

import Development.Shake
import Development.Spaceport.Common.Tools
import System.Process.Runner

data Aapt
  = AaptPackage
    { manifestPath :: FilePath
    , resourcePath :: FilePath
    , includePaths :: [FilePath]
    , packagePath :: FilePath
    }
  | AaptPackageResources
    { manifestPath :: FilePath
    , resourcePath :: FilePath
    , includePaths :: [FilePath]
    , assetsPaths :: [FilePath]
    , releaseMode :: Bool
    , packagePath :: FilePath
    }
  | AaptCrunch
    { resourcePaths :: [FilePath]
    , packagePath :: FilePath
    }
  | AaptAddResources
    { resourcePaths :: [FilePath]
    , packagePath :: FilePath
    }

runAaptAction :: ExeRunner Action -> Runner Action Aapt
runAaptAction run aapt = case aapt of
  AaptPackage {..} -> do
    need [manifestPath]  -- TODO resourcePath, includePaths?
    run $ concat
      [ arg "package"
      , flag "-f"  -- Force overwrite.
      , flag "-m"  -- Make package.
      , option "-M" manifestPath
      , option "-S" resourcePath
      , options "-I" includePaths
      , option "-J" packagePath
      , flag "--generate-dependencies"
      ]

  AaptPackageResources {..} -> do
    need [manifestPath]  -- TODO resourcePath, includePaths, assetsPaths?
    run $ concat
      [ arg "package"
      , flag "-f"  -- Force overwrite.
      , flag "--no-crunch"
      , flagIf (not releaseMode) "--debug-mode"
      , option "-M" manifestPath
      , option "-S" resourcePath
      , options "-I" includePaths
      , options "-A" assetsPaths
      , option "-F" packagePath
      , flag "--generate-dependencies"
      ]

  AaptCrunch {..} -> do
    need []  -- TODO resourcePaths?
    run $ concat
      [ arg "crunch"
      , options "-S" resourcePaths
      , option "-C" packagePath
      ]

  AaptAddResources {..} -> do
    need [packagePath]  -- TODO resourcePaths?
    run $ concat
      [ arg "add"
      , arg packagePath
      , args resourcePaths
      ]
