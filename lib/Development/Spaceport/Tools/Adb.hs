{-# LANGUAGE RecordWildCards #-}

module Development.Spaceport.Tools.Adb
  ( Adb(..)
  , mkAdb
  , runAdbIO
  ) where

import Development.Spaceport.Common.Tools
import System.Process.Runner

data Adb = Adb
  { serialNumber :: Maybe String
  , command :: String
  , arguments :: [Parameter]
  }

mkAdb :: String -> [Parameter] -> Adb
mkAdb c a = Adb
  { serialNumber = Nothing
  , command = c
  , arguments = a
  }

runAdbIO :: ExeRunner IO -> Runner IO Adb
runAdbIO run Adb {..}
  = run $ concat
  $ [ maybeOption "-s" serialNumber
    , arg command
    ] ++ arguments
