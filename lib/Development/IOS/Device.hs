{-# LANGUAGE RecordWildCards #-}

module Development.IOS.Device
  ( UUID(UUID)

  , deviceName
  , connectedDevices
  , connectedDeviceNames
  , deviceForName
  , installIPA
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class

import qualified Control.Monad.Parallel as Par

import Development.Spaceport.RunTool
import Development.Spaceport.Tools.IDeviceID
import Development.Spaceport.Tools.IDeviceInstaller
import Development.Spaceport.Util
import System.Process.Runner

newtype UUID = UUID { unUUID :: String }
  deriving (Eq, Ord, Show)

connectedDevices
  :: (MonadIO m) => Runner m IDeviceID -> m [UUID]
connectedDevices iDeviceID
  = liftM (uuidsFromOutput . fst)
  . runHidden "list connected iOS devices"
  $ iDeviceID ListDevices
  where
    uuidsFromOutput
      = map UUID . filter (not . null) . ioLines
      . bytestringToStringUTF8

connectedDeviceNames
  :: (Par.MonadParallel m, MonadIO m)
  => Runner m IDeviceID
  -> m [String]
connectedDeviceNames config
  = Par.mapM (deviceName config)
  =<< connectedDevices config

deviceName
  :: (MonadIO m) => Runner m IDeviceID -> UUID -> m String
deviceName iDeviceID (UUID uuid)
  = liftM (headIOLine . bytestringToStringUTF8 . fst)
  . runHidden "get iOS device name"
  $ iDeviceID (DeviceName uuid)

deviceForName
  :: (Par.MonadParallel m, MonadIO m)
  => Runner m IDeviceID
  -> String
  -> m (Maybe UUID)
deviceForName config name = do
  devices <- connectedDevices config
  liftM msum . flip Par.mapM devices $ \ uuid -> do
    n <- deviceName config uuid
    return $ justIf (n == name) uuid

installIPA
  :: (MonadIO m)
  => Runner m IDeviceInstaller
  -> Maybe UUID
  -> FilePath
  -> m ()
installIPA iDeviceInstaller mUuid ipaPath
  = runHidden_ "install IPA on iOS device"
  $ iDeviceInstaller IDeviceInstaller
      { uuid = unUUID <$> mUuid
      , ipaPath = ipaPath
      }
