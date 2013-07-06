{-# LANGUAGE RecordWildCards #-}

module Development.Spaceport.Tools.ZipAlign
  ( ZipAlign(..)
  , runZipAlignAction
  ) where

import Development.Shake
import Development.Spaceport.Common.Tools
import System.Process.Runner

data ZipAlign = ZipAlign
  { inputZip :: FilePath
  , outputZip :: FilePath
  }

runZipAlignAction :: ExeRunner Action -> Runner Action ZipAlign
runZipAlignAction run ZipAlign {..} = do
  need [inputZip]
  run $ concat
    [ option "-f" "4"  -- Align on 4-byte boundaries.
    , arg inputZip
    , arg outputZip
    ]
