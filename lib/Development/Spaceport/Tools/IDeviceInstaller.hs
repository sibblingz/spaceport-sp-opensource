{-# LANGUAGE RecordWildCards #-}

module Development.Spaceport.Tools.IDeviceInstaller
  ( IDeviceInstaller(..)
  , runIDeviceInstallerIO
  ) where

import Development.Spaceport.Common.Tools
import System.Process.Runner

data IDeviceInstaller = IDeviceInstaller
  { uuid :: Maybe String
  , ipaPath :: FilePath
  }

runIDeviceInstallerIO :: ExeRunner IO -> Runner IO IDeviceInstaller
runIDeviceInstallerIO run IDeviceInstaller {..}
  = run $ concat
    [ maybeOption "-U" uuid
    , option "-i" ipaPath
    ]
