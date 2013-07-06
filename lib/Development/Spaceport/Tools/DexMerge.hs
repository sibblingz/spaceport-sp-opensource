{-# LANGUAGE RecordWildCards #-}

module Development.Spaceport.Tools.DexMerge
  ( DexMerge(..)
  , dexMergeClass
  , runDexMergeAction
  ) where

import Development.Shake
import System.Java
import System.Process.Runner

data DexMerge = DexMerge
  { firstDex :: FilePath
  , secondDex :: FilePath
  , mergedDex :: FilePath
  }

dexMergeClass :: JavaClass
dexMergeClass = "com.android.dx.merge.DexMerger"

runDexMergeAction
  :: ExeRunner Action
  -> Runner Action DexMerge
runDexMergeAction run DexMerge {..} = do
  need [firstDex, secondDex]
  run [mergedDex, firstDex, secondDex]
