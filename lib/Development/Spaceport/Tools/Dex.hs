{-# LANGUAGE RecordWildCards #-}

module Development.Spaceport.Tools.Dex
  ( Dex(..)
  , dexClass
  , runDexAction
  ) where

import Development.Shake
import Development.Spaceport.Common.Tools
import System.Java
import System.Process.Runner

data Dex = Dex
  { inputClass :: FilePath
  , outputDex :: FilePath
  }

dexClass :: JavaClass
dexClass = "com.android.dx.command.Main"

runDexAction
  :: ExeRunner Action
  -> Runner Action Dex
runDexAction run Dex {..} = do
  need [inputClass]
  run $ concat
    [ flag "--dex"
    , flag "--no-strict"
    , option "--output" outputDex
    , [inputClass]
    ]
