{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

module Development.Android.Adb
  ( SerialNumber(SerialNumber)

  , shell
  , install
  , getProperty
  , connectedDevices
  , connectedDeviceNames
  , startServer
  , deviceName
  , deviceForName
  ) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.List (stripPrefix)
import Data.Maybe
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Typeable

import qualified Control.Monad.Parallel as Par
import qualified Data.ByteString as BS
import qualified Data.Text as Text

import Development.Spaceport.Common.Tools
import Development.Spaceport.RunTool
import Development.Spaceport.Tools.Adb hiding (command)
import Development.Spaceport.Util
import System.Process.Runner

newtype SerialNumber = SerialNumber { unSerial :: String }
  deriving (Eq, Ord, Show)

shell
  :: (MonadIO m)
  => Runner m Adb
  -> Maybe SerialNumber
  -> String
  -> [String]
  -> m BS.ByteString
shell adb serial command commandArgs = liftM fst
  . runHidden ("run ADB shell " ++ command)
  $ adb (mkAdb "shell" [[command], commandArgs])
    { serialNumber = unSerial <$> serial }

data InstallFailed = InstallFailed Text
  deriving (Typeable)

instance Show InstallFailed where
  show (InstallFailed message)
    = "Install to Android device failed: "
    ++ Text.unpack message

instance Exception InstallFailed where

install
  :: (MonadIO m)
  => Runner m Adb
  -> Maybe SerialNumber
  -> FilePath
  -> m ()
install adb serial apkPath = do
  (out, _err) <- runHidden ("install " ++ apkPath)
    $ adb (mkAdb "install" [flag "-r", arg apkPath])
      { serialNumber = unSerial <$> serial }
  liftIO . maybe (return ()) throwIO $ getInstallFailure out
  return ()

-- | Checks if an 'adb install' failed by inspecting its
-- standard output.  'adb install' does not give proper exit
-- codes, so this is the only way to determine if an
-- installation succeeded or not.
getInstallFailure :: BS.ByteString -> Maybe InstallFailed
getInstallFailure output 
  = case reverse . ioLines $ bytestringToStringUTF8 output of
    (lastLine:_)
      | lastLine == "Success" -> Nothing
      | Just reason <- stripPrefix "Failure " lastLine
        -> Just $ InstallFailed (Text.pack reason)
      | otherwise -> Just . InstallFailed
        $ "Unknown failure (" <> Text.pack lastLine <> ")"
    [] -> Just $ InstallFailed "Unknown failure (no output)"

getProperty
  :: (MonadIO m)
  => Runner m Adb
  -> Maybe SerialNumber
  -> String
  -> m String
getProperty adb serial propName
  = liftM (headIOLine . bytestringToStringUTF8)
  $ shell adb serial "getprop" [propName]

connectedDevices
  :: (MonadIO m)
  => Runner m Adb
  -> m [SerialNumber]
connectedDevices adb
  = liftM (parseDeviceList . bytestringToStringUTF8 . fst)
  . runHidden "get connected Android devices"
  $ adb (mkAdb "devices" [])

connectedDeviceNames
  :: (Par.MonadParallel m, MonadIO m)
  => Runner m Adb
  -> m [String]
connectedDeviceNames adb
  = Par.mapM (deviceName adb . Just)
  =<< connectedDevices adb

parseDeviceList :: String -> [SerialNumber]
parseDeviceList input = case ioLines input of
  (_:xs) -> mapMaybe parseDeviceLine xs
  [] -> []
  where
    parseDeviceLine line = case words line of
      [serial, _deviceStatus] -> Just $ SerialNumber serial
      _ -> Nothing

startServer
  :: (MonadIO m)
  => Runner m Adb
  -> m ()
startServer adb
  = runHidden_ "start ADB server"
  $ adb (mkAdb "start-server" [])

deviceName
  :: (MonadIO m)
  => Runner m Adb
  -> Maybe SerialNumber
  -> m String
deviceName adb serial
  = getProperty adb serial "ro.product.model"

deviceForName
  :: (Par.MonadParallel m, MonadIO m)
  => Runner m Adb
  -> String
  -> m (Maybe SerialNumber)
deviceForName adb name = do
  devices <- connectedDevices adb
  liftM msum . flip Par.mapM devices $ \ serial -> do
    n <- deviceName adb (Just serial)
    return $ justIf (n == name) serial
