{-# LANGUAGE RecordWildCards #-}

module Development.Spaceport.Tools.IDeviceID
  ( IDeviceID(..)
  , runIDeviceIDIO
  ) where

import System.Process.Runner

data IDeviceID
  = ListDevices
  | DeviceName String

runIDeviceIDIO :: ExeRunner IO -> Runner IO IDeviceID
runIDeviceIDIO run ListDevices
  = run ["-l"]
runIDeviceIDIO run (DeviceName uuid)
  = run [uuid]
