{-# LANGUAGE RecordWildCards #-}

module Settings
  ( globalSettingsPath

  , ProjectType(..)
  , SettingsSource(..)
  , settingsFromSource
  , settingsFromSources

  , applySettings
  , applyFirstAvailableSettings
  ) where

import Control.Applicative
import Data.Maybe
import Data.Monoid
import System.Environment.XDG.BaseDir
import System.FilePath

import qualified Data.Set as Set

import Data.SpaceportSettings (Settings)
import Development.ActionScript3.Error
import Development.ActionScript3.Project
import Text.JavaProperties

import qualified Data.SpaceportSettings as Settings
import qualified Development.ActionScript3.FlashBuilder as FlashBuilder
import qualified Development.ActionScript3.FlashDevelop as FlashDevelop

globalSettingsPath :: IO FilePath
globalSettingsPath = do
  configDir <- getUserConfigDir "spaceport"
  return $ configDir </> "settings"

data ProjectType
  = FlashBuilderProject
  | FlashDevelopProject
  deriving (Eq, Ord)

data SettingsSource
  = ProjectFile (Maybe ProjectType) FilePath
  | SettingsFile FilePath
  | JustSettings Settings
  deriving (Eq, Ord)

settingsFromProject :: Project -> Settings
settingsFromProject Project {..} = Settings.empty
  { Settings.sourcePaths = Just
    $ Set.fromList projectSourceDirs
  , Settings.entryPoint = Just projectAS3EntryPoint
  , Settings.swcLibraryDirs = Just
    $ Set.fromList projectLibraryDirs
  , Settings.swcLibraryFiles = Just
    $ Set.fromList projectLibraryFiles
  }

readAnyProject
  :: FilePath
  -> IO (Either String (Project, [ProjectWarning]))
readAnyProject path = do
  flashBuilder <- FlashBuilder.readProject path
  flashDevelop <- FlashDevelop.readProject path
  return $ case (flashBuilder, flashDevelop) of
    (Left _, Right fd) -> Right fd
    (Right fb, Left _) -> Right fb
    (Left errFB, Left errFD) -> Left $ concat
      [ "Error reading as Flash Builder project: ", errFB
      , "; error reading as FlashDevelop project: ", errFD
      ]
    (Right _, Right _) -> Left
      "Project can be read as both FlashBuilder\
      \ and FlashDevelop project"

settingsFromSource
  :: SettingsSource
  -> IO (Settings, [ProjectFileWarning])
settingsFromSource source = case source of
  ProjectFile projectType path -> do
    mProject <- case projectType of
      Just FlashBuilderProject -> FlashBuilder.readProject path
      Just FlashDevelopProject -> FlashDevelop.readProject path
      Nothing -> readAnyProject path
    case mProject of
      Left err -> fail err  -- TODO Something nicer.
      Right (project, warnings) -> do
        let
          warnings' = map (ProjectFileWarning path) warnings
          settings = settingsFromProject project
        return (settings, warnings')
  SettingsFile p -> do
    result <- Settings.mergeFile mempty p
    return (result, [])
  JustSettings s -> return (s, [])

-- | Reads settings from multiple sources.  Values of
-- sources at the head of the list take precedence.
settingsFromSources
  :: [SettingsSource]
  -> IO (Settings, [ProjectFileWarning])
settingsFromSources
  = fmap (mconcat . reverse)
  . mapM settingsFromSource

-- | Finds the first file for which *any* (not all) of the
-- given properties have been configured.
findFileWithAnyProperty
  :: Set.Set Key -> [FilePath] -> IO (Maybe FilePath)
findFileWithAnyProperty keys = findPropertiesFile
  $ any ((`Set.member` keys) . fst) . filePairs

-- | Finds the first properties file for which the given
-- predicate returns 'True'.
findPropertiesFile
  :: (File -> Bool) -> [FilePath] -> IO (Maybe FilePath)
findPropertiesFile _ [] = return Nothing
findPropertiesFile f (path:paths)
  = readPropertiesFileOrEmpty path >>= \ prop
    -> if f prop
      then return $ Just path
      else findPropertiesFile f paths

applySettings :: Settings -> FilePath -> IO ()
applySettings settings = mapPropertiesFile modify
  where
    modify :: File -> File
    modify file = foldr (uncurry setValue) file
      $ Settings.settingValues settings

-- | Applies settings to the last property file in a list
-- which will keep those settings effective.
applyFirstAvailableSettings :: Settings -> [FilePath] -> IO FilePath
applyFirstAvailableSettings settings paths@(defaultPath:_) = do
  path <- fromMaybe defaultPath
    <$> findFileWithAnyProperty keys paths
  applySettings settings path
  return path
  where
    keys = Set.fromList . map fst
      $ Settings.settingValues settings
applyFirstAvailableSettings _ []
  -- The non-totality here is bugging me; we should accept a
  -- non-empty list.
  = fail "Cannot apply settings: no settings files available"
