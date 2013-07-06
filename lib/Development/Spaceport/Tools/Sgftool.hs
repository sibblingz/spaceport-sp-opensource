{-# LANGUAGE RecordWildCards #-}

module Development.Spaceport.Tools.Sgftool
  ( Sgftool(..)
  , runSgftoolAction
  ) where

import Development.Shake
import System.Process.Runner

data Sgftool = Sgftool
  { inputSwf :: FilePath
  , outputSgf :: FilePath
  }

runSgftoolAction :: ExeRunner Action -> Runner Action Sgftool
runSgftoolAction run Sgftool {..} = do
  need [inputSwf]
  run [inputSwf, outputSgf]
