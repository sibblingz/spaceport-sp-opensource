{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Sp
  ( Sp(..)
  , runSp

  , Options(..)
  , getOptions
  , getOption

  , getToolsIO
  , getToolsAction
  , getSupport

  , getSettings
  , getSettingsSources
  , getProjectSettingsSources
  , projectSettingsFile
  , ensureSingleProjectFile
  ) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader hiding (local)
import Data.SpaceportSettings
import Data.Typeable
import Development.ActionScript3.Error
import Development.Shake.FilePath
import Development.Spaceport.Support
import Development.Spaceport.SupportPaths
import Settings
import System.Directory

#ifdef USE_NAILGUN
import Control.Concurrent
import Data.List
import System.IO
import System.Java.Nailgun
import System.Process
#endif

import qualified Data.SpaceportSettings as Settings
import qualified Development.ActionScript3.FlashBuilder as FlashBuilder
import qualified Development.ActionScript3.FlashDevelop as FlashDevelop
import qualified Development.Shake as Shake

#ifdef USE_NAILGUN
import qualified System.Java as Java
import qualified System.Java.Locate as Java
#endif

newtype Sp a = Sp (ReaderT (Options, SpState) IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

runSp :: Sp a -> Options -> IO a
runSp (Sp m) options = do
  state <- newSpState
  runReaderT m (options, state)
    `finally` cleanUp state
  where
#ifdef USE_NAILGUN
    newSpState = SpState <$> newMVar Nothing
    cleanUp SpState {..} = do
      mServer <- takeMVar nailgunServer
      case mServer of
        Nothing -> return ()
        Just (_server, pid) -> terminateProcess pid
#else
    newSpState = pure SpState
    cleanUp (SpState {}) = return ()
#endif

data Options = Options
  { verbose :: Bool
  , parallelJobs :: Int
  , keepGoing :: Bool
  , buildDirectory :: FilePath
  , ignoreMissingProjectFile :: Bool
  , projectFile :: Maybe FilePath
  , localSettings :: Settings
  }

data SpState = SpState
#ifdef USE_NAILGUN
  { nailgunServer :: MVar (Maybe (NailgunServer, ProcessHandle)) }
#endif

getOptions :: Sp Options
getOptions = Sp $ asks fst

getOption :: (Options -> a) -> Sp a
getOption f = f <$> getOptions

#ifdef USE_NAILGUN
nailgunClassPaths :: Sp [ClassPath]
nailgunClassPaths = do
  SupportPaths {..} <- getSupportPaths
  -- Extend as needed for optimization purposes.
  return [spSignerPath]

getIOToolRunner :: Sp (ToolRunner IO)
getIOToolRunner = do
  SupportPaths {..} <- getSupportPaths
  spState <- Sp $ asks snd
  ngClassPaths <- nailgunClassPaths
  jvmPath <- liftIO Java.getJVMPath

  return ToolRunner
    { runExe = \ exePath args
      -> runInteractiveProcess exePath args Nothing Nothing
    , runJava = \ classPaths javaClass args -> do
      (server, _pid) <- getNailgunServer
        nailgunServerPath jvmPath spState ngClassPaths
      runJVMOrNG
        nailgunClientPath server ngClassPaths
        classPaths jvmPath javaClass args
    }

-- | Launch Nailgun server if not already started.
--
-- Each invocation must have the same class paths.
getNailgunServer
  :: NailgunServerPath
  -> JavaPath
  -> SpState
  -> [ClassPath]
  -> IO (NailgunServer, ProcessHandle)
getNailgunServer ngServerPath jvmPath SpState {..} classPaths
  = memoizedWithMVar nailgunServer
    $ runNewNailgunServer ngServerPath jvmPath classPaths

memoizedWithMVar :: MVar (Maybe a) -> IO a -> IO a
memoizedWithMVar var m
  = modifyMVar var
    $ maybe m return
      >=> \ value -> return (Just value, value)

runNewNailgunServer
  :: NailgunServerPath
  -> JavaPath
  -> [ClassPath]
  -> IO (NailgunServer, ProcessHandle)
runNewNailgunServer nailgunServerPath jvmPath classPaths = do
  (server, out, err, pid)
    <- runServer jvmPath nailgunServerPath
      hostName portNumber [] classPaths environment
  hClose out
  hClose err
  return (server, pid)
  where
    hostName = Nothing
    portNumber = Nothing
    environment = Nothing

-- | Runs a class using Nailgun if possible, or in a new JVM
-- instance otherise.
runJVMOrNG
  :: NailgunClientPath
  -> NailgunServer
  -> [ClassPath]  -- ^ Class paths running on server.
  -> [ClassPath]  -- ^ Class paths desired.
  -> JavaPath
  -> JavaClass
  -> [String]
  -> IO (Handle, Handle, Handle, ProcessHandle)
runJVMOrNG ngClient ngServer ngClassPath classPath jvmPath
  | classPath `setIncludedIn` ngClassPath
    = runClient ngClient ngServer
  | otherwise
    = Java.runJVM jvmPath [] classPath cwd env
  where cwd = Nothing ; env = Nothing

setIncludedIn :: (Eq a, Ord a) => [a] -> [a] -> Bool
xs `setIncludedIn` ys
  = length (xs `intersect` ys) == length xs
#else
getIOToolRunner :: Sp (ToolRunner IO)
getIOToolRunner = liftIO ioToolRunner
#endif

getActionToolRunner :: Sp (ToolRunner Shake.Action)
getActionToolRunner
  = actionRunnerFromIORunner <$> getIOToolRunner

getToolsIO :: Sp (Tools IO)
getToolsIO = toolsIOFromPaths
  <$> getIOToolRunner <*> getSupportPaths

getToolsAction :: Sp (Tools Shake.Action)
getToolsAction = toolsActionFromPaths
  <$> getActionToolRunner <*> getSupportPaths

getSupport :: Sp Support
getSupport = supportFromPaths <$> getSupportPaths

data MissingSupportDirectory = MissingSupportDirectory
  deriving (Typeable)

instance Show MissingSupportDirectory where
  show MissingSupportDirectory = "Missing support directory"

instance Exception MissingSupportDirectory where

getSupportPaths :: Sp SupportPaths
getSupportPaths = do
  supportPaths <- getSupportPaths'
    >>= maybe (liftIO $ throwIO MissingSupportDirectory) return
  liftIO $ validateSupportPaths supportPaths
  return supportPaths

getSupportPaths' :: Sp (Maybe SupportPaths)
getSupportPaths' = do
  (settings, _warnings) <- getSettings' False
  case Settings.supportPath settings of
    Just path -> return . Just
      $ supportPathsFromRoot path
    Nothing -> liftIO guessSupportPaths

getSettings :: Sp (Settings, [ProjectFileWarning])
getSettings = do
  ignore <- getOption ignoreMissingProjectFile
  getSettings' (not ignore)

getSettingsSources :: Sp [SettingsSource]
getSettingsSources = do
  local <- JustSettings <$> getOption localSettings
  projects <- getProjectSettingsSources
  global <- SettingsFile <$> liftIO globalSettingsPath
  return $ [local] ++ projects ++ [global]

getProjectSettingsSources :: Sp [SettingsSource]
getProjectSettingsSources = do
  mProjectFile <- getOption projectFile
  case mProjectFile of
    Just file -> return [ProjectFile Nothing file]
    Nothing -> liftIO $ do
      projectDirectory <- getCurrentDirectory
      fbProjects <- map
        (ProjectFile (Just FlashBuilderProject))
        <$> FlashBuilder.listProjects projectDirectory
      fdProjects <- map
        (ProjectFile (Just FlashDevelopProject))
        <$> FlashDevelop.listProjects projectDirectory
      let allProjects = fbProjects ++ fdProjects
      return
        $ [ SettingsFile (projectSettingsFile p)
          | ProjectFile _ p <- allProjects
          ] ++ allProjects

projectSettingsFile :: FilePath -> FilePath
projectSettingsFile = (<.> "spaceportSettings")

getSettings' :: Bool -> Sp (Settings, [ProjectFileWarning])
getSettings' errorOnMissingProjectFile = do
  sources <- getSettingsSources
  when errorOnMissingProjectFile
    . void . liftIO $ ensureSingleProjectFile sources
  liftIO $ settingsFromSources sources

data ProjectError
  = MissingProject
  | MultipleProjects [(Maybe ProjectType, FilePath)]
  deriving (Typeable)

instance Show ProjectError where
  show MissingProject
    -- TODO Base error message off of available project
    -- types.
    = "Missing '.actionScriptProperties' project file"

  show (MultipleProjects projects)
    = "Multiple project files found, but only one is allowed:\n"
      ++ unlines (map (("  " ++) . snd) projects)
      ++ "Use --project-file to pick one"

instance Exception ProjectError where

ensureSingleProjectFile
  :: [SettingsSource] -> IO SettingsSource
ensureSingleProjectFile sources
  = case [(pType, path) | ProjectFile pType path <- sources] of
    [] -> throwIO MissingProject
    [(projectType, path)] -> return $ ProjectFile projectType path
    projects -> do
      currentDirectory <- getCurrentDirectory
      throwIO . MultipleProjects
        $ map (fmap (makeRelative currentDirectory)) projects
