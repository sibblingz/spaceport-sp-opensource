{-# LANGUAGE RecordWildCards #-}

module Development.Spaceport.Tools.A2J
  ( A2J(..)
  , A2JLibrary(..)
  , runA2JAction
  ) where

import Development.Shake
import System.Process.Runner

import Development.ShakeExtras
import Development.Spaceport.Common.Tools
import Development.Spaceport.Util

-- [Note A2J library output]:
--
-- The library output path given to A2J refers to the
-- runtime path of the SGF file (in SWF form).  This path
-- must be a relative URI to the game's index file
-- (`index.html`).  That is, it cannot be absolute, and uses
-- '/' to separate paths.

data A2J = A2J
  { sourcePathDirs :: [FilePath]
  , entryPoint :: String
  , libraries :: [A2JLibrary]
  , outputFile :: String
  , depFile :: FilePath
  }

data A2JLibrary = A2JLibrary
  { librarySWF :: FilePath
  , libraryOutput :: String
  -- ^ See [note A2J library output].
  }

runA2JAction :: ExeRunner Action -> Runner Action A2J
runA2JAction run a2j@A2J {..} = do
  asFiles <- concatMapM (`getFilesDeep` "*.as") sourcePathDirs
  need $ asFiles ++ map librarySWF libraries
  run $ a2jOptions a2j

a2jOptions :: A2J -> [String]
a2jOptions A2J {..} = concat
  [ options "--search-path" sourcePathDirs
  , option "--output" outputFile
  , flag "--file-output"
  , options "--library" $ map libraryOption libraries
  , option "--embed-output" depFile
  , arg entryPoint
  ]

libraryOption :: A2JLibrary -> String
libraryOption A2JLibrary {..}
  = librarySWF ++ "=" ++ libraryOutput
