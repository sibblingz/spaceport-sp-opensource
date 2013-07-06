{-# LANGUAGE RecordWildCards #-}

module Development.Spaceport.Tools.Swc2Swf
  ( Swc2Swf(..)
  , swc2SwfClass
  , runSwc2SwfAction
  ) where

import Development.Shake
import System.Java
import System.Process.Runner

data Swc2Swf = Swc2Swf
  { inputSwc :: FilePath
  , outputSwf :: FilePath
  }

swc2SwfClass :: JavaClass
swc2SwfClass = "io.spaceport.Swc2Swf"

runSwc2SwfAction :: ExeRunner Action -> Runner Action Swc2Swf
runSwc2SwfAction run Swc2Swf {..} = do
  need [inputSwc]
  run [inputSwc, outputSwf]
