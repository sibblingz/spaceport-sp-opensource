{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.SpaceportSettings
  ( Orientation(..)
  , allOrientations

  , AndroidPlugin(..)
  , parseAndroidPluginName

  , Settings(..)
  , empty

  , merge
  , mergeFile

  , SettingDesc(..)
  , SettingType(..)
  , SettingVisibility(..)
  , allSettingDescs
  , visibleSettingDescs
  , settingDescByKey

  , KeyComments(..)
  , defaultSettingsText

  , parseFile
  , parseFile'
  , parseFileOrEmpty
  , parseText

  , settingValues
  ) where

import Control.Applicative hiding (empty)
import Control.Monad
import Data.ApplicationID (ApplicationID)
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Set (Set)
import Data.Text (Text)
import System.Directory
import System.FilePath ((<.>))
import Text.Parsec.Pos (SourceName)

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Development.Spaceport.Util
import Text.JavaProperties

import qualified Data.ApplicationID as ApplicationID
import qualified Data.IOS.InfoPlist as IOS

data AndroidPlugin = AndroidPlugin
  { androidPluginPackage :: [Text]
  , androidPluginName :: Text
  } deriving (Eq, Ord, Show)

data Orientation = Landscape | Portrait
  deriving (Eq, Ord, Show, Enum, Bounded)

allOrientations :: Set Orientation
allOrientations = fullSet

data Settings = Settings
  { applicationID               :: Maybe ApplicationID
  , version                     :: Maybe Text
  , versionCode                 :: Maybe Text
  , authorizationKey            :: Maybe Text
  , displayName                 :: Maybe Text
  , entryPoint                  :: Maybe FilePath
  , urlSchemes                  :: Maybe (Set Text)
  , iOSAddIconGloss             :: Maybe Bool
  , iOSAppRunsInBackground      :: Maybe Bool
  , iOSDeviceFamilies           :: Maybe (Set IOS.DeviceFamily)
  , iOSResourcesPath            :: Maybe FilePath
  , iOSDevIdentity              :: Maybe String
  , iOSDevMobileProvisionFile   :: Maybe String
  , swcLibraryDirs              :: Maybe (Set FilePath)
  , swcLibraryFiles             :: Maybe (Set FilePath)
  , orientations                :: Maybe (Set Orientation)
  , projectRoot                 :: Maybe FilePath
  , sourcePaths                 :: Maybe (Set FilePath)
  , loadingScreenPath           :: Maybe FilePath
  , keystorePath                :: Maybe FilePath
  , keyAlias                    :: Maybe Text
  , keystorePasswordPath        :: Maybe FilePath
  , keyPasswordPath             :: Maybe FilePath
  , androidResourcesPath        :: Maybe FilePath
  , supportPath                 :: Maybe FilePath
  , appDescription              :: Maybe Text
  } deriving (Eq, Ord, Show)

empty :: Settings
empty = Settings
  Nothing
  Nothing
  Nothing
  Nothing
  Nothing
  Nothing
  Nothing
  Nothing
  Nothing
  Nothing
  Nothing
  Nothing
  Nothing
  Nothing
  Nothing
  Nothing
  Nothing
  Nothing
  Nothing
  Nothing
  Nothing
  Nothing
  Nothing
  Nothing
  Nothing
  Nothing

-- | Combines two settings, preferring options from the
-- right.
merge :: Settings -> Settings -> Settings
merge a b = Settings
  -- Use 'choose' to override, and 'combine' to merge.
  { applicationID               = choose applicationID a b
  , version                     = choose version a b
  , versionCode                 = choose versionCode a b
  , authorizationKey            = choose authorizationKey a b
  , displayName                 = choose displayName a b
  , entryPoint                  = choose entryPoint a b
  , urlSchemes                  = combine urlSchemes a b
  , iOSAddIconGloss             = choose iOSAddIconGloss a b
  , iOSAppRunsInBackground      = choose iOSAppRunsInBackground a b
  , iOSDeviceFamilies           = choose iOSDeviceFamilies a b
  , iOSResourcesPath            = choose iOSResourcesPath a b
  , iOSDevMobileProvisionFile   = choose iOSDevMobileProvisionFile a b
  , iOSDevIdentity              = choose iOSDevIdentity a b
  , swcLibraryDirs              = combine swcLibraryDirs a b
  , swcLibraryFiles             = combine swcLibraryFiles a b
  , orientations                = combine orientations a b
  , projectRoot                 = choose projectRoot a b
  , sourcePaths                 = combine sourcePaths a b
  , loadingScreenPath           = choose loadingScreenPath a b
  , keystorePath                = choose keystorePath a b
  , keyAlias                    = choose keyAlias a b
  , keystorePasswordPath        = choose keystorePasswordPath a b
  , keyPasswordPath             = choose keyPasswordPath a b
  , androidResourcesPath        = choose androidResourcesPath a b
  , supportPath                 = choose supportPath a b
  , appDescription              = choose appDescription a b
  }

instance Monoid Settings where
  mempty = empty
  mappend = merge

choose :: Alternative f => (a -> f b) -> a -> a -> f b
choose f a b = f b <|> f a

combine :: Monoid b => (a -> b) -> a -> a -> b
combine f a b = f b <> f a

-- | Merge a file into settings if it exists; keeps settings
-- the same otherwise.
mergeFile :: Settings -> FilePath -> IO Settings
mergeFile base path = ifM (doesFileExist path)
  (merge base <$> parseFile' path)
  (return base)

data KeyComments = CommentKeys | IncludeKeys

data SettingVisibility = Visible | Internal
  deriving (Eq)

data SettingType
  = FilePath (Maybe Text)  -- ^ File with extension.
  | DirectoryPath
  | String
  | Enumerated [Text]
  | ManySpaceSeparated SettingType

boolType, orientationsType, iOSDeviceFamiliesType :: SettingType
boolType = Enumerated $ enumerate showHumanBool
orientationsType = ManySpaceSeparated
  . Enumerated $ enumerate showOrientation
iOSDeviceFamiliesType = ManySpaceSeparated
  . Enumerated $ enumerate showIOSDeviceFamily

enumerate :: (Bounded a, Enum a) => (a -> b) -> [b]
enumerate f = map f [minBound .. maxBound]

data SettingDesc = SettingDesc
  { keyName :: Text
  , description :: Text
  , exampleValue :: Text
  , settingVisibility :: SettingVisibility
  , parseValue :: Text -> Either String Settings
  , showValue :: Settings -> Maybe Text
  , settingType :: SettingType
  }

newtype SpaceSeparated a = SpaceSeparated (Set a)

class ParseSettingValue a where
  parseSettingValue :: Text -> Either String a

instance ParseSettingValue FilePath where
  parseSettingValue = Right . Text.unpack
instance ParseSettingValue Text where
  parseSettingValue = Right
instance ParseSettingValue Bool where
  parseSettingValue
    = maybe (Left "'yes' or 'no'") Right
    . readHumanBool
instance ParseSettingValue ApplicationID where
  parseSettingValue = mapLeft
    (\ errs -> "Invalid application ID: " ++ andList (map show errs))
    . ApplicationID.fromText
instance ParseSettingValue Orientation where
  parseSettingValue = maybeToEither "Invalid orientation"
    . readOrientation
instance ParseSettingValue IOS.DeviceFamily where
  parseSettingValue = maybeToEither "Invalid iOS device family"
    . readIOSDeviceFamily
instance (ParseSettingValue a, Ord a) => ParseSettingValue (SpaceSeparated a) where
  parseSettingValue = fmap (SpaceSeparated . Set.fromList)
    . mapM parseSettingValue . Set.toList . wordSet

class ShowSettingValue a where
  showSettingValue :: a -> Text

instance ShowSettingValue FilePath where
  showSettingValue = Text.pack
instance ShowSettingValue Text where
  showSettingValue = id
instance ShowSettingValue Bool where
  showSettingValue = showHumanBool
instance ShowSettingValue ApplicationID where
  showSettingValue = ApplicationID.toText
instance ShowSettingValue Orientation where
  showSettingValue = showOrientation
instance ShowSettingValue IOS.DeviceFamily where
  showSettingValue = showIOSDeviceFamily
instance (ShowSettingValue a, Ord a) => ShowSettingValue (SpaceSeparated a) where
  showSettingValue (SpaceSeparated xs)
    = Text.unwords . map showSettingValue $ Set.toList xs

settingDesc
  :: (ParseSettingValue a, ShowSettingValue a)
  => Text  -- ^ Key name.
  -> Text  -- ^ Description.
  -> Text  -- ^ Example value.
  -> SettingVisibility
  -> SettingType
  -> (Settings -> a -> Settings)  -- ^ Embed.
  -> (Settings -> Maybe a)        -- ^ Extract.
  -> SettingDesc
settingDesc
  keyName' description' exampleValue' settingVisibility'
  settingType' embed extract = SettingDesc
  { keyName = keyName'
  , description = description'
  , exampleValue = exampleValue'
  , settingVisibility = settingVisibility'
  , parseValue = fmap (embed empty) . parseSettingValue
  , showValue = fmap showSettingValue . extract
  , settingType = settingType'
  }

allSettingDescs :: [SettingDesc]
allSettingDescs

    -- Common settings.
  = [ settingDesc
      "id"
      "Application ID"
      "io.spaceport.untitled"
      Visible
      String
      (\ s x -> s { applicationID = Just x })
      applicationID

    , settingDesc
      "display_name"
      "Application display name"
      "Untitled Game"
      Visible
      String
      (\ s x -> s { displayName = Just x })
      displayName

    , settingDesc
      "version"
      "Version number"
      "1.0.0"
      Visible
      String
      (\ s x -> s { version = Just x })
      version

    , settingDesc
      "version_code"
      "Internal Android integer version code"
      "100"
      Visible
      String
      (\ s x -> s { versionCode = Just x })
      versionCode

    , settingDesc
      "authorization_key"
      "Spaceport CDN authorization key"
      "..."
      Visible
      String
      (\ s x -> s { authorizationKey = Just x })
      authorizationKey

    , settingDesc
      "orientations"
      "Space-separated list of allowed device orientations"
      "portrait landscape"
      Visible
      orientationsType
      (\ s (SpaceSeparated x) -> s { orientations = Just x })
      (fmap SpaceSeparated . orientations)

    , settingDesc
      "loading_screen_file"
      "Path to loading screen file"
      (genericFile "swf")
      Visible
      (FilePath (Just "swf"))
      (\ s x -> s { loadingScreenPath = Just x })
      loadingScreenPath

    , settingDesc
      "url_schemes"
      "Space-separated list of custom URL schemes"
      "myscheme"
      Visible
      (ManySpaceSeparated String)
      (\ s (SpaceSeparated x) -> s { urlSchemes = Just x })
      (fmap SpaceSeparated . urlSchemes)

    -- iOS settings.

    , settingDesc
      "ios_dev_identity"
      "iOS development identity (.p12 or name)"
      (genericFile "p12")
      Visible
      String
      (\ s x -> s { iOSDevIdentity = Just x })
      iOSDevIdentity

    , settingDesc
      "ios_dev_mobile_provision_file"
      "Path to iOS development mobile provision file"
      (genericFile "mobileprovision")
      Visible
      (FilePath (Just "mobileprovision"))
      (\ s x -> s { iOSDevMobileProvisionFile = Just x })
      iOSDevMobileProvisionFile

    , settingDesc
      "ios_resources"
      "iOS resources directory"
      genericDirectory
      Visible
      DirectoryPath
      (\ s x -> s { iOSResourcesPath = Just x })
      iOSResourcesPath

    , settingDesc
      "ios_device_families"
      "Supported iOS device families"
      "ipad iphone"
      Visible
      iOSDeviceFamiliesType
      (\ s (SpaceSeparated x) -> s { iOSDeviceFamilies = Just x })
      (fmap SpaceSeparated . iOSDeviceFamilies)

    , settingDesc
      "ios_add_icon_gloss"
      "Enable iOS icon auto-gloss"
      "yes"
      Visible
      boolType
      (\ s x -> s { iOSAddIconGloss = Just x })
      iOSAddIconGloss

    , settingDesc
      "ios_app_runs_in_background"
      "Enable iOS application to run in the background"
      "yes"
      Visible
      boolType
      (\ s x -> s { iOSAppRunsInBackground = Just x })
      iOSAppRunsInBackground

    -- Android settings.

    , settingDesc
      "keystore_file"
      "Path to Android keystore file"
      (genericFile "keystore")
      Visible
      (FilePath (Just "keystore"))
      (\ s x -> s { keystorePath = Just x })
      keystorePath

    , settingDesc
      "key_alias"
      "Alias of Android release build key"
      "mykey"
      Visible
      String
      (\ s x -> s { keyAlias = Just x })
      keyAlias

    , settingDesc
      "keystore_password_file"
      "Path to file containing Android keystore password"
      (genericFile "")
      Visible
      (FilePath Nothing)
      (\ s x -> s { keystorePasswordPath = Just x })
      keystorePasswordPath

    , settingDesc
      "key_password_file"
      "Path to file containing Android key password"
      (genericFile "")
      Visible
      (FilePath Nothing)
      (\ s x -> s { keyPasswordPath = Just x })
      keyPasswordPath

    , settingDesc
      "android_resources"
      "Path to Android 'res' directory\nSee: http://developer.android.com/guide/topics/resources/providing-resources.html"
      genericDirectory
      Visible
      DirectoryPath
      (\ s x -> s { androidResourcesPath = Just x })
      androidResourcesPath

    -- For testing only.

    , settingDesc
      "support_path"
      "Path to Spaceport SDK support directory"
      ""
      Internal
      DirectoryPath
      (\ s x -> s { supportPath = Just x })
      supportPath

    , settingDesc
      "source_path"
      ""
      ""
      Internal
      DirectoryPath
      (\ s x -> s { sourcePaths = Just $ Set.singleton x })
      printingUnsupported

    , settingDesc
      "entry_point"
      "AS3 entry point file"
      ""
      Internal
      (FilePath (Just "as"))
      (\ s x -> s { entryPoint = Just x })
      entryPoint

    , settingDesc
      "project_root"
      ""
      ""
      Internal
      DirectoryPath
      (\ s x -> s { projectRoot = Just x })
      projectRoot

    , settingDesc
      "library_file"
      ""
      ""
      Internal
      (FilePath (Just "swc"))
      (\ s x -> s { swcLibraryFiles = Just $ Set.singleton x })
      printingUnsupported

    , settingDesc
      "library_dir"
      ""
      ""
      Internal
      DirectoryPath
      (\ s x -> s { swcLibraryDirs = Just $ Set.singleton x })
      printingUnsupported

    , settingDesc
      "app_description"
      ""
      ""
      Internal
      String
      (\ s x -> s { appDescription = Just x })
      appDescription
    ]

  where
    genericFile ext = Text.pack $ "/path/to/file" <.> ext
    genericDirectory = "/path/to/directory/"
    printingUnsupported _settings = Nothing

visibleSettingDescs :: [SettingDesc]
visibleSettingDescs
  = filter ((== Visible) . settingVisibility) allSettingDescs

settingDescByKey :: Text -> Maybe SettingDesc
settingDescByKey key
  = find ((== key) . keyName) allSettingDescs

defaultSettingsText :: KeyComments -> Text
defaultSettingsText keys
  = Text.unlines $ map descriptionText visibleSettingDescs
  where
    comment :: Text -> Text
    comment = Text.unlines . map (mappend "# ") . Text.lines

    descriptionText :: SettingDesc -> Text
    descriptionText SettingDesc {..} = Text.unlines
      [ comment description
      , key $ keyName <> " = " <> exampleValue
      ]

    key = case keys of
      CommentKeys -> comment
      IncludeKeys -> id

wordSet :: Text -> Set Text
wordSet = Set.fromList . Text.words

readOrientation :: Text -> Maybe Orientation
readOrientation s
  | s `caselessEq` "landscape" = Just Landscape
  | s `caselessEq` "portrait" = Just Portrait
  | otherwise = Nothing

showOrientation :: Orientation -> Text
showOrientation Landscape = "landscape"
showOrientation Portrait = "portrait"

readIOSDeviceFamily :: Text -> Maybe IOS.DeviceFamily
readIOSDeviceFamily s
  | s `caselessEq` "iphone" = Just IOS.IPhone
  | s `caselessEq` "ipad" = Just IOS.IPad
  | otherwise = Nothing

showIOSDeviceFamily :: IOS.DeviceFamily -> Text
showIOSDeviceFamily IOS.IPhone = "iphone"
showIOSDeviceFamily IOS.IPad = "ipad"

fromConfig :: [(Text, Text)] -> Settings
fromConfig config = mconcat $ mapMaybe parseDesc allSettingDescs
  where
    parseDesc SettingDesc {..} = do
      strValue <- lookup keyName config
      eitherToMaybe $ parseValue strValue

parseFile :: FilePath -> IO (Either String Settings)
parseFile path = do
  contents <- Text.readFile path
  return $ parseText' path contents

parseFile' :: FilePath -> IO Settings
parseFile' = unEitherString <=< parseFile

-- | Attempts to parse a settings file; if parsing fails,
-- returns an empty 'Settings'.
parseFileOrEmpty :: FilePath -> IO Settings
parseFileOrEmpty path = ifM (doesFileExist path)
  (parseFile' path)
  (return empty)

parseText :: Text -> Either String Settings
parseText = parseText' ""

parseText' :: SourceName -> Text -> Either String Settings
parseText' source
  = either (Left . show) (Right . fromConfig . filePairs)
  . parseProperties source

parseAndroidPluginName :: Text -> Maybe AndroidPlugin
parseAndroidPluginName name = case Text.splitOn "." name of
  xs@(_:_:_) -> Just $ AndroidPlugin (init xs) (last xs)
  _ -> Nothing

settingValues :: Settings -> [(Text, Text)]
settingValues settings = mapMaybe kvp allSettingDescs
  where
    kvp SettingDesc {..} = (,) keyName <$> showValue settings
