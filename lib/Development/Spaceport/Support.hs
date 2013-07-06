{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module Development.Spaceport.Support
  ( ToolRunner(..)
  , ioToolRunner
  , actionToolRunner
  , actionRunnerFromIORunner

  , Tools(..)
  , toolsIOFromPaths
  , toolsActionFromPaths

  , Support(..)
  , supportFromPaths
  ) where

import Control.Monad
import Development.Shake
import Development.Spaceport.SupportPaths
import Development.Spaceport.Tools
import System.FilePath
import System.Process
import System.Process.Runner

import qualified System.Java as Java
import qualified System.Java.Locate as Java

data ToolRunner m = ToolRunner
  { runExe :: FilePath -> ExeRunner m
  , runJava :: [Java.ClassPath] -> Java.JavaClass -> ExeRunner m
  }

ioToolRunner :: IO (ToolRunner IO)
ioToolRunner = do
  jvmPath <- Java.getJVMPath
  return ToolRunner
    { runExe = \ exePath args
      -> runInteractiveProcess exePath args cwd env
    , runJava = \ classPaths
      -> Java.runJVM jvmPath [] classPaths cwd env
    }
  where cwd = Nothing ; env = Nothing

actionToolRunner :: IO (ToolRunner Action)
actionToolRunner
  = fmap actionRunnerFromIORunner ioToolRunner

actionRunnerFromIORunner :: ToolRunner IO -> ToolRunner Action
actionRunnerFromIORunner r = ToolRunner
  { runExe = \ exePath args -> do
    when (isAbsolute exePath) $ need [exePath]
    liftIO $ runExe r exePath args
  , runJava = \ classPaths javaClass args -> do
    need classPaths
    liftIO $ runJava r classPaths javaClass args
  }

data Tools m = Tools
  -- Core
  { a2j :: Runner m A2J
  , sgftool :: Runner m Sgftool
  , simulator :: Runner m Simulator
  , spSigner :: Runner m SpSigner
  , spsx :: Runner m Spsx
  , swc2Swf :: Runner m Swc2Swf
  -- iOS
  , iDeviceID :: Runner m IDeviceID
  , iDeviceInstaller :: Runner m IDeviceInstaller
  -- Android
  , aapt :: Runner m Aapt
  , adb :: Runner m Adb
  , dex :: Runner m Dex
  , dexMerge :: Runner m DexMerge
  , zipAlign :: Runner m ZipAlign
  }

data Support = Support
  -- Core
  { a2jDefsDir :: FilePath
  , defaultLoadingScreenPath :: FilePath
  , spaceportLibraryPath :: FilePath
  , unrequirePath :: FilePath
  -- iOS
  , unsignedSpaceportiOSBundleDebug :: FilePath
  , unsignedSpaceportiOSBundleRelease :: FilePath
  , iOSDefaultResourcesPath :: FilePath
  -- Android
  , androidDebugExtraPath :: FilePath
  , androidDefaultResourcesPath :: FilePath
  , androidJarPath :: FilePath
  , androidReleaseExtraPath :: FilePath
  , androidRequiredResourcesPath :: FilePath
  , androidSpaceportDex :: FilePath
  }

toolsIOFromPaths
  :: ToolRunner IO -> SupportPaths -> Tools IO
toolsIOFromPaths ToolRunner {..} SupportPaths {..} = Tools
  { a2j = unsupported "a2j"
  , sgftool = unsupported "sgftool"
  , simulator = runSimulatorIO $ runExeLiveIO simulatorExecutable
  , spSigner = runSpSignerIO $ runJava [spSignerPath] spSignerClass
  , spsx = unsupported "spsx"
  , swc2Swf = unsupported "swc2Swf"
  , iDeviceID = runIDeviceIDIO $ runExe iDeviceIDPath
  , iDeviceInstaller = runIDeviceInstallerIO $ runExe iDeviceInstallerPath
  , aapt = unsupported "aapt"
  , adb = runAdbIO $ runExe adbPath
  , dex = unsupported "dex"
  , dexMerge = unsupported "dexMerge"
  , zipAlign = unsupported "zipAlign"
  }
  where
    unsupported name = fail
      $ "INTERNAL ERROR: Unsupported IO: " ++ name

    simulatorExecutable
#ifdef OSX_TOOLS
      = simulatorPath </> "Contents/MacOS/SpaceportSimulator"
#else
      = simulatorPath
#endif

toolsActionFromPaths
  :: ToolRunner Action -> SupportPaths -> Tools Action
toolsActionFromPaths ToolRunner {..} SupportPaths {..} = Tools
  { a2j = runA2JAction $ runExe a2jPath
  , sgftool = runSgftoolAction $ runExe sgftoolPath
  , simulator = unsupported "simulator"
  , spSigner = runSpSignerAction $ runJava [spSignerPath] spSignerClass
  , spsx = runSpsxAction $ runExe spsxPath
  , swc2Swf = runSwc2SwfAction $ runJava [swc2SwfPath] swc2SwfClass
  , iDeviceID = unsupported "iDeviceID"
  , iDeviceInstaller = unsupported "iDeviceInstaller"
  , aapt = runAaptAction $ runExe aaptPath
  , adb = unsupported "adb"
  , dex = runDexAction $ runJava [dexPath] dexClass
  , dexMerge = runDexMergeAction $ runJava [dexPath] dexMergeClass
  , zipAlign = runZipAlignAction $ runExe zipAlignPath
  }
  where
    unsupported name = fail
      $ "INTERNAL ERROR: Unsupported Action: " ++ name

-- Note [Run simulator]:
--
-- In order to get the simulator to output live, we must not
-- create a pipe for stdout or stderr.  If we do, the
-- simulator buffers output.  One can use forkpty(3) but
-- this solution is good enough.
--
-- To add onto this hack, we need to run
-- SpaceportSimulator.exe such that its working directory is
-- the directory containing SpaceportSimulator.exe (so it
-- can find curl.exe for making crash reports).

-- | See note [Run simulator].
runExeLiveIO :: FilePath -> ExeRunner IO
runExeLiveIO exePath args = liftM
  (\ pid -> (err, err, err, pid))
  $ runProcess exePath args cwd Nothing
    Nothing Nothing Nothing
  where
    err = error "Development.Spaceport.Support.runExeLive"
    cwd = Just (dropFileName exePath)

supportFromPaths :: SupportPaths -> Support
supportFromPaths SupportPaths {..} = Support
  { a2jDefsDir = a2jDefsDir
  , defaultLoadingScreenPath = defaultLoadingScreenPath
  , spaceportLibraryPath = spaceportLibraryPath
  , unrequirePath = unrequirePath
  , unsignedSpaceportiOSBundleDebug = unsignedSpaceportiOSBundleDebug
  , unsignedSpaceportiOSBundleRelease = unsignedSpaceportiOSBundleRelease
  , iOSDefaultResourcesPath = iOSDefaultResourcesPath
  , androidSpaceportDex = androidSpaceportDexPath
  , androidJarPath = androidJarPath
  , androidDebugExtraPath = androidDebugExtraPath
  , androidReleaseExtraPath = androidReleaseExtraPath
  , androidRequiredResourcesPath = androidRequiredResourcesPath
  , androidDefaultResourcesPath = androidDefaultResourcesPath
  }
