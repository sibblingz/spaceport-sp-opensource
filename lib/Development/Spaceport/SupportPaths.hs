{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module Development.Spaceport.SupportPaths
  ( SupportPaths(..)
  , guessSupportPaths
  , supportPathsFromRoot

  , IncompleteSupportDirectory(..)
  , validateSupportPaths
  , checkSupportPaths
  ) where

import Control.Exception
import Control.Monad
import Data.Maybe
import Data.Typeable
import Development.Spaceport.Util
import System.Directory
import System.Environment.ExecutablePath
import System.FilePath

data SupportPaths = SupportPaths
  -- Core
  { sgftoolPath :: FilePath
  , a2jPath :: FilePath
  , a2jDefsDir :: FilePath
  , defaultLoadingScreenPath :: FilePath
#ifdef USE_NAILGUN
  , nailgunClientPath :: FilePath
  , nailgunServerPath :: FilePath
#endif
  , spSignerPath :: FilePath
  , spsxPath :: FilePath
  , spaceportLibraryPath :: FilePath
  , swc2SwfPath :: FilePath
  , unrequirePath :: FilePath
  -- iOS
  , unsignedSpaceportiOSBundleDebug :: FilePath
  , unsignedSpaceportiOSBundleRelease :: FilePath
  , simulatorPath :: FilePath
  , iDeviceIDPath :: FilePath
  , iDeviceInstallerPath :: FilePath
  , iOSDefaultResourcesPath :: FilePath
  -- Android
  , aaptPath :: FilePath
  , dexPath :: FilePath
  , zipAlignPath :: FilePath
  , adbPath :: FilePath
  , androidDebugExtraPath :: FilePath
  , androidDefaultResourcesPath :: FilePath
  , androidJarPath :: FilePath
  , androidReleaseExtraPath :: FilePath
  , androidRequiredResourcesPath :: FilePath
  , androidSpaceportDexPath :: FilePath
  }

guessSupportPaths :: IO (Maybe SupportPaths)
guessSupportPaths = do
  exePath <- liftM dropFileName getExecutablePath
  dirPaths <- filterM doesDirectoryExist
    $ guessSupportDirPaths exePath
  return . fmap supportPathsFromRoot
    $ listToMaybe dirPaths

guessSupportDirPaths :: FilePath -> [FilePath]
guessSupportDirPaths root
  = [ root </> "support"
    , root </> "spaceport-support"
    , root </> ".." </> "lib" </> "spaceport-support"
    ]

exeExt, appExt :: String
#if defined(WINDOWS_TOOLS)
exeExt = "exe"
appExt = "exe"
#elif defined(OSX_TOOLS)
exeExt = ""
appExt = "app"
#elif defined(LINUX_TOOLS)
exeExt = ""
appExt = ""
#elif defined(HLINT)
exeExt = "invalid"
appExt = "invalid"
#else
#error "Don't know how to configure tools for unknown platform"
#endif

supportPathsFromRoot :: FilePath -> SupportPaths
supportPathsFromRoot root = SupportPaths
  { sgftoolPath = root </> "sgftool" <.> exeExt
  , a2jPath = root </> "a2j/a2j" <.> exeExt
  , a2jDefsDir = root </> "a2j/defs"
  , defaultLoadingScreenPath = root </> "default_loading_screen.swf"
#ifdef USE_NAILGUN
  , nailgunClientPath = root </> "nailgun/ng" <.> exeExt
  , nailgunServerPath = root </> "nailgun/nailgun-server.jar"
#endif
  , spSignerPath = root </> "sp-signer/sp-signer.jar"
  , spsxPath = root </> "spsx" <.> exeExt
  , spaceportLibraryPath = root </> "js/spaceport.js"
  , swc2SwfPath = root </> "swc2swf.jar"
  , unrequirePath = root </> "js/unrequire.js"

  , unsignedSpaceportiOSBundleDebug = root </> "ios/unsigned-ios-dbg.app"
  , unsignedSpaceportiOSBundleRelease = root </> "ios/unsigned-ios-rel.app"
  , simulatorPath = root </> "SpaceportSimulator" <.> appExt
  , iDeviceInstallerPath = root </> "ios/libimobiledevice/ideviceinstaller" <.> exeExt
  , iDeviceIDPath = root </> "ios/libimobiledevice/idevice_id" <.> exeExt
  , iOSDefaultResourcesPath = root </> "ios/res-default"

  , aaptPath = root </> "android/aapt" <.> exeExt
  , dexPath = root </> "android/dx.jar"
  , zipAlignPath = root </> "android/zipalign" <.> exeExt
  , adbPath = root </> "android/adb" <.> exeExt
  , androidSpaceportDexPath = root </> "android/classes.dex"
  , androidJarPath = root </> "android/android.jar"
  , androidDebugExtraPath = root </> "android/debug/extra.zip"
  , androidReleaseExtraPath = root </> "android/release/extra.zip"
  , androidRequiredResourcesPath = root </> "android/res"
  , androidDefaultResourcesPath = root </> "android/res-default"
  }

data IncompleteSupportDirectory
  = IncompleteSupportDirectory [FilePath]
  deriving (Typeable)

instance Show IncompleteSupportDirectory where
  show (IncompleteSupportDirectory missingFiles)
    = unlines $ ["Support directory incomplete; try reinstalling the Spaceport SDK"]
      ++ map ("Missing: " ++) missingFiles

instance Exception IncompleteSupportDirectory where

validateSupportPaths :: SupportPaths -> IO ()
validateSupportPaths paths = do
  missing <- checkSupportPaths paths
  unless (null missing)
    . throwIO $ IncompleteSupportDirectory missing

checkSupportPaths :: SupportPaths -> IO [FilePath]
checkSupportPaths SupportPaths {..}
  = liftM2 (++)
    (liftM catMaybes $ mapM checkFile files)
    (liftM catMaybes $ mapM checkDirectory directories)

  where
    checkFile p = liftM (`justUnless` p) $ doesFileExist p
    checkDirectory p = liftM (`justUnless` p) $ doesDirectoryExist p
    justUnless b = justIf (not b)

    files
      = [ a2jPath
        , aaptPath
        , adbPath
        , androidDebugExtraPath
        , androidJarPath
        , androidReleaseExtraPath
        , androidSpaceportDexPath
        , defaultLoadingScreenPath
        , dexPath
        , iDeviceIDPath
        , iDeviceInstallerPath
#ifdef USE_NAILGUN
        , nailgunClientPath
        , nailgunServerPath
#endif
        , sgftoolPath
        , spSignerPath
        , spaceportLibraryPath
        , spsxPath
        , swc2SwfPath
        , unrequirePath
        , zipAlignPath
#ifndef OSX_TOOLS
        , simulatorPath
#endif
        ]

    directories
      = [ a2jDefsDir
        , androidDefaultResourcesPath
        , androidRequiredResourcesPath
        , iOSDefaultResourcesPath
        , unsignedSpaceportiOSBundleDebug
        , unsignedSpaceportiOSBundleRelease
#ifdef OSX_TOOLS
        , simulatorPath
#endif
        ]
