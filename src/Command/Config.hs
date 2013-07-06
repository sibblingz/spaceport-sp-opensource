{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Command.Config
  ( Config(..)
  , parse
  , exec
  ) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import Data.Typeable
import Options.Applicative hiding (value)
import Options.Applicative.Types
import Options.Applicative.Builder.Internal (CommandFields, FlagFields, Mod)

import qualified Data.Foldable as Foldable
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Command.Common
import Settings
import Sp
import Text.JavaProperties

import qualified Data.SpaceportSettings as Settings

data ConfigFile
  = Project
  | Global
  | File FilePath

data Config = Config (Maybe ConfigFile) ConfigSubCommand

data ConfigSubCommand
  = ListConfig
  | ShowConfig (Maybe Settings.SettingDesc)
  | SetConfig Settings.SettingDesc Text
  | DeleteConfig Settings.SettingDesc
  | DescribeConfig Settings.SettingDesc
  | PrintConfigPath

parseConfigKey :: String -> Maybe Settings.SettingDesc
parseConfigKey = Settings.settingDescByKey . Text.pack

parseConfigValue :: String -> Maybe Text
parseConfigValue = Just . Text.pack

commandFlag :: Mod FlagFields () -> Parser ()
commandFlag = flag' ()

-- | Like 'bashCompleter'
-- (http://hackage.haskell.org/packages/archive/optparse-applicative/0.5.2.1/doc/html/Options-Applicative-Builder-Completer.html#v:bashCompleter),
-- but handles '-'-prefixed words properly.
bashCompleter' :: String -> Completer
bashCompleter' act = Completer $ \ word
  -> runCompleter (bashCompleter act) ("-- " ++ word)

cmd
  :: String  -- ^ Command name.
  -> String  -- ^ Description.
  -> Parser a
  -> Mod CommandFields a
cmd name desc parser
  = command name . info parser $ mconcat
    [ fullDesc
    , progDesc desc
    ]

parse :: CommandParser Config
parse f
  = cmd "config" "Manipulates Spaceport settings entries."
  . f $ Config
    <$> optional parseConfigFile
    <*> Foldable.asum subCommands

  where
    parseConfigFile = Foldable.asum
      [ Project <$ commandFlag (mconcat
        [ long "project"
        , help "Use project .spaceportSettings file."
        ])

      , Global <$ commandFlag (mconcat
        [ long "global"
        , help "Use global settings file."
        ])

      , File <$> strOption (mconcat
        [ short 'f'
        , long "file"
        , help "Use given settings file."
        , hidden
        ])
      ]

    subCommands
      = [ SetConfig <$> configKey <*> configValue

        , ShowConfig <$ flag' () (mconcat
          [ long "show"
          , help "Display configured settings."
          ]) <*> optional configKey

        , ListConfig <$ flag' () (mconcat
          [ long "list"
          , help "List available settings."
          ])

        , DescribeConfig <$ flag' () (mconcat
          [ long "describe"
          , help "Describe a setting."
          ]) <*> configKey

        , DeleteConfig <$ flag' () (mconcat
          [ long "delete"
          , help "Delete a setting."
          ]) <*> configKey

        , PrintConfigPath <$ flag' () (mconcat
          [ long "path"
          , help "Print the path of the settings file."
          ])
        ]

    configKey = argument parseConfigKey $ mconcat
      [ metavar "KEY"
      , completeWith keyNames
      , help "Setting name. Use `sp config --list` to list all settings."
      ]

    keyNames = map
      (Text.unpack . Settings.keyName)
      Settings.visibleSettingDescs

    configValue = Text.unwords
      <$> arguments1 parseConfigValue (mconcat
      [ metavar "VALUE"
      , completer $ bashCompleter' "file"
      ])

data InvalidSettingsValue
  = InvalidSettingsValue Settings.SettingDesc Text String
  deriving (Typeable)

instance Show InvalidSettingsValue where
  show (InvalidSettingsValue _desc _value err)
    = "Invalid settings value: " ++ err

instance Exception InvalidSettingsValue where

execSubCommand
  :: Sp FilePath           -- ^ Get settings file.
  -> Sp Settings.Settings  -- ^ Get settings.
  -> ConfigSubCommand
  -> Sp ()
execSubCommand getSettingsPath readSettings subCommand = case subCommand of
  ShowConfig Nothing -> do
    settings <- readSettings
    liftIO . forM_ Settings.visibleSettingDescs $ \ Settings.SettingDesc {..}
      -> case showValue settings of
        Just value -> Text.putStrLn $ keyName <> " = " <> value
        Nothing -> return ()

  ShowConfig (Just Settings.SettingDesc {..}) -> do
    settings <- readSettings
    liftIO . Text.putStrLn . fromMaybe Text.empty
      $ showValue settings

  ListConfig -> liftIO . forM_ Settings.visibleSettingDescs
    $ \ Settings.SettingDesc {..} -> Text.putStrLn
      $ keyName <> " - " <> head (Text.lines description)

  DescribeConfig Settings.SettingDesc {..}
    -> liftIO . Text.putStr $ Text.unlines
      [ "Setting name: " <> keyName
      , "Description: " <> description
      , "Type: " <> showType settingType
      ]
      where
        showType (Settings.FilePath (Just ext))
          = "." <> ext <> " file path"
        showType (Settings.FilePath Nothing)
          = "File path"
        showType Settings.DirectoryPath
          = "Directory path"
        showType Settings.String = "String"
        showType (Settings.Enumerated options)
          = "One of: " <> Text.unwords options
        showType (Settings.ManySpaceSeparated (Settings.Enumerated options))
          = "One or more of: " <> Text.unwords options
        showType (Settings.ManySpaceSeparated xs)
          = "One or more of: " <> showType xs

  SetConfig desc@Settings.SettingDesc {..} value -> do
    case parseValue value of
      Left err -> liftIO . throwIO
        $ InvalidSettingsValue desc value err
      Right _ -> return ()
    settingsPath <- getSettingsPath
    liftIO $ mapPropertiesFile (setValue keyName value) settingsPath

  DeleteConfig Settings.SettingDesc {..} -> do
    settingsPath <- getSettingsPath
    liftIO $ mapPropertiesFile (deleteKey keyName) settingsPath

  PrintConfigPath -> liftIO . putStrLn =<< getSettingsPath

exec :: CommandExecutor Config
exec (Config mFile subCommand) = do
  (settingsPath, settings) <- case mFile of
    Just file -> do
      path <- case file of
        Project -> getProjectSettingsPath
        Global -> liftIO globalSettingsPath
        File path -> return path
      return (return path, liftIO $ Settings.parseFileOrEmpty path)
    Nothing -> return (getProjectSettingsPath, settings)
      where settings = liftM fst getSettings

  execSubCommand settingsPath settings subCommand

  where
    getProjectSettingsPath = do
      ProjectFile _ projectPath
        <- liftIO . ensureSingleProjectFile
          =<< getProjectSettingsSources
      return $ projectSettingsFile projectPath
