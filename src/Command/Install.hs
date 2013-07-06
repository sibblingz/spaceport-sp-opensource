{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Command.Install
  ( Install(..)
  , InstallTarget(..)
  , parse
  , exec
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Data.Monoid
import Data.Set (Set)
import Development.Shake.FilePath
import Options.Applicative
import Options.Applicative.Types (Completer)

import qualified Control.Monad.Parallel as Par
import qualified Data.Set as Set
import qualified Development.Shake as Shake

import CheckBuild
import Command.Common
import Development.Spaceport.Android.Target
import Development.Spaceport.BuildConfig
import Development.Spaceport.BuildTarget
import Development.Spaceport.Core.Target
import Development.Spaceport.IOS.Target
import Development.Spaceport.Support
import Development.Spaceport.Tools.Adb hiding (command)
import Development.Spaceport.Tools.IDeviceID
import Development.Spaceport.Util
import ModeConfig
import Sp
import System.Process.Runner

import qualified Development.Android.Adb as Android
import qualified Development.IOS.Device as IOS

data Install = Install
  { targetTypes :: Set InstallTargetType
  , modeConfig :: ModeConfig
  }

data InstallTargetType = IOSTarget | AndroidTarget
  deriving (Eq, Ord, Enum, Bounded)

installTargetTypes :: [InstallTargetType]
installTargetTypes = [minBound .. maxBound]

instance Show InstallTargetType where
  show IOSTarget = "ios"
  show AndroidTarget = "android"

readInstallTargetType :: String -> Maybe InstallTargetType
readInstallTargetType "ios" = Just IOSTarget
readInstallTargetType "android" = Just AndroidTarget
readInstallTargetType _ = Nothing

data InstallTarget
  = IOS IOS.UUID
  | Android Android.SerialNumber
  deriving (Eq)

getUuid :: InstallTarget -> Maybe IOS.UUID
getUuid t = case t of { IOS uuid -> Just uuid ; _ -> Nothing }

getSerial :: InstallTarget -> Maybe Android.SerialNumber
getSerial t = case t of { Android s -> Just s ; _ -> Nothing }

showInstallTarget
  :: Runner IO IDeviceID
  -> Runner IO Adb
  -> InstallTarget
  -> IO String
showInstallTarget iDeviceID adb target = case target of
  Android serial -> Android.deviceName adb (Just serial)
  IOS device -> IOS.deviceName iDeviceID device

deviceNameCompleter :: Completer
deviceNameCompleter = listCompleter
  $ map show installTargetTypes

parse :: CommandParser Install
parse f = command "install" . info parseArgs $ mconcat
  [ fullDesc
  , progDesc "Installs an application onto a USB-connected mobile device."
  ]
  where
    parseArgs = f $ Install
      <$> Set.fromList <$> arguments1 readInstallTargetType (mconcat
        [ help $ "Device to install to. " ++ orList (map show installTargetTypes) ++ "."
        , metavar "DEVICE"
        , completer deviceNameCompleter
        ])
      <*> parseModeConfig

installTargetFile :: ModeConfig -> InstallTarget -> FilePath
installTargetFile ModeConfig {..} target = case target of
  IOS _ -> getPath $ IOSFile iOSMode "built.ipa"
  Android _ -> getPath $ AndroidFile androidMode "built.apk"
  where
    manifestMode = ManifestMode jsMode modeApplicationSource
    iOSMode = IOSMode manifestMode modeSpaceportBinary modeSigningMode
    androidMode = AndroidMode manifestMode modeSpaceportBinary modeSigningMode
    jsMode = JSMode modeOptimizationLevel

getInstallTarget
  :: Runner IO IDeviceID
  -> Runner IO Adb
  -> InstallTargetType
  -> IO [InstallTarget]
getInstallTarget iDeviceID _ IOSTarget = do
  iOSDevices <- IOS.connectedDevices iDeviceID
  case iOSDevices of
    [] -> fail "No iOS devices connected"
    _ -> return $ map IOS iOSDevices
getInstallTarget _ adb AndroidTarget = do
  Android.startServer adb
  androidDevices <- Android.connectedDevices adb
  case androidDevices of
    [] -> fail "No Android devices connected"
    _ -> return $ map Android androidDevices

exec :: CommandExecutor Install
exec Install {..} = do
  tools@Tools {..} <- getToolsIO

  installTargets <- liftIO
    . concatMapM (getInstallTarget iDeviceID adb)
    $ Set.toList targetTypes
  names <- liftIO $ Par.mapM (showInstallTarget iDeviceID adb) installTargets
  liftIO $ case names of
    [] -> fail "Need a device to install to."
    [name] -> putStrLn $ "Preparing to install to " ++ name ++ "..."
    _ -> putStrLn $ "Preparing to install to " ++ show (length names)
      ++ " devices: " ++ andList names

  let uuids = mapMaybe getUuid installTargets
  let serials = mapMaybe getSerial installTargets
  unless (null uuids) $ ensureiOSBuildable spSigner uuids
  unless (null serials) ensureAndroidBuildable

  buildDir <- getOption buildDirectory
  runShake . Shake.want
    $ map ((buildDir </>) . installTargetFile modeConfig) installTargets

  liftIO $ do
    parallelMapM_ (installTarget tools buildDir) installTargets
    putStrLn $ "Successfully installed to " ++ andList names ++ "."

  where
    installTarget Tools {..} buildDir target = case target of
      IOS device -> IOS.installIPA iDeviceInstaller (Just device) path
      Android serial -> Android.install adb (Just serial) path
      where path = buildDir </> installTargetFile modeConfig target

parallelMapM_
  :: (Par.MonadParallel m)
  => (a -> m b) -> [a] -> m ()
parallelMapM_ f = Par.sequence_ . map f
