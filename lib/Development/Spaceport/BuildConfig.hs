{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Development.Spaceport.BuildConfig
  ( ProjectConfig(..)
  , IOSConfig(..)
  , AndroidConfig(..)
  , AndroidSigningConfig(..)
  , ModeConfig(..)
  , spaceportOutputPath

  , fromSettings
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Android.PackageName (PackageName)
import Data.Maybe
import Data.Set (Set)
import Data.Text (Text)
import System.Directory
import System.FilePath

import qualified Data.Set as Set
import qualified Data.SpaceportSettings as Settings
import qualified Data.Text as Text

import Development.IOS.Identity
import Development.Spaceport.BuildTarget
import Development.Spaceport.IOS.Settings
import Development.Spaceport.Tools.SpSigner
import Development.Spaceport.Util
import System.Environment.Expand
import System.IO.Prompt.Password
import System.Process.Runner

import qualified Data.Android.PackageName as PackageName
import qualified Data.ApplicationID as ApplicationID
import qualified Data.IOS.BundleID as BundleID
import qualified Data.IOS.InfoPlist as IOS
import qualified Development.Spaceport.Android.Manifest as Android
import qualified Development.Spaceport.Manifest as Manifest

data AndroidConfig = AndroidConfig
  { androidDisplayName :: Text
  , androidLoadingScreenPath :: Maybe FilePath
  , androidOrientation :: Android.Orientation
  , androidPackageName :: PackageName
  , androidPermissions :: Set Android.Permission
  , androidPlugins :: [Settings.AndroidPlugin]
  , androidReleaseMode :: Bool
  , androidResourcesPath :: Maybe FilePath
  , androidSigningConfig :: AndroidSigningConfig
  , androidVersion :: Text
  , androidVersionCode :: Text
  , androidURLSchemes :: Set Text
  }

data AndroidSigningConfig = AndroidSigningConfig
  { androidKeyAlias :: Text
  , androidKeyPassword :: IO Text
  , androidKeystorePassword :: IO Text
  , androidKeystorePath :: Maybe FilePath
  }

data IOSConfig = IOSConfig
  { iOSIdentity :: Maybe Identity
  , iOSMobileProvisionFile :: Maybe FilePath
  , iOSBundleSettings :: IOS.Settings
  , iOSResourcesPath :: Maybe FilePath
  }

data ProjectConfig = ProjectConfig
  { projectInputPaths :: [FilePath]
  , projectEntryPoint :: FilePath
  , projectLibraryDirs :: [FilePath]
  , projectLibraryFiles :: [FilePath]
  , projectLoadingScreenFile :: Maybe FilePath
  , projectDescription :: Text
  }

data ModeConfig = ModeConfig
  { modeOptimizationLevel :: OptimizationLevel
  , modeApplicationSource :: ApplicationSource
  , modeSpaceportBinary :: SpaceportBinary
  , modeSigningMode :: SigningMode
  }

spaceportOutputPath :: FilePath
spaceportOutputPath = "spaceport"

orientationToiOSOrientations
  :: Settings.Orientation -> Set IOS.Orientation
orientationToiOSOrientations o = Set.fromList $ case o of
  Settings.Landscape -> [IOS.LandscapeLeft, IOS.LandscapeRight]
  Settings.Portrait  -> [IOS.Portrait, IOS.PortraitUpsideDown]

orientationsToAndroidOrientation
  :: Set Settings.Orientation -> Android.Orientation
orientationsToAndroidOrientation orientations
  | hasBoth      = Android.FullSensor
  | hasLandscape = Android.Landscape
  | hasPortrait  = Android.Portrait
  | otherwise    = Android.FullSensor
  where
    hasBoth      = hasLandscape && hasPortrait
    hasLandscape = Settings.Landscape `Set.member` orientations
    hasPortrait  = Settings.Portrait  `Set.member` orientations

fromSettings
  :: Runner IO SpSigner
  -> Settings.Settings
  -> IO (ProjectConfig, IOSConfig, AndroidConfig, Manifest.CDNConfig)
fromSettings signer settings = do
  entryPoint <- unMaybe "Missing entry point." $ Settings.entryPoint settings
  sourcePaths <- unMaybe "Missing source paths." $ Settings.sourcePaths settings

  projectRoot <- maybe
    (liftIO getCurrentDirectory) return
    $ Settings.projectRoot settings
  let projectFile = liftM (projectRoot </>) . expandPath

  projectLibraryDirs' <- mapM projectFile . maybeSetToList
    $ Settings.swcLibraryDirs settings
  projectLibraryFiles' <- mapM projectFile . maybeSetToList
    $ Settings.swcLibraryFiles settings
  projectLoadingScreenFile' <- projectFile
    $$ Settings.loadingScreenPath settings
  iOSResourcesPath' <- projectFile
    $$ Settings.iOSResourcesPath settings
  androidLoadingScreenPath' <- projectFile
    $$ Settings.loadingScreenPath settings
  androidKeystorePath' <- projectFile
    $$ Settings.keystorePath settings

  iOSDevMobileProvisionFile <- projectFile
    $$ Settings.iOSDevMobileProvisionFile settings
  iOSDevIdentity <- identityFromString signer
    $$ Settings.iOSDevIdentity settings

  return
    ( ProjectConfig
      { projectInputPaths = Set.toList sourcePaths
      , projectEntryPoint = entryPoint
      , projectLibraryDirs = projectLibraryDirs'
      , projectLibraryFiles = projectLibraryFiles'
      , projectLoadingScreenFile = projectLoadingScreenFile'
      , projectDescription = fromMaybe ""
        $ Settings.appDescription settings
      }

    , IOSConfig
      { iOSIdentity = iOSDevIdentity
      , iOSMobileProvisionFile = iOSDevMobileProvisionFile

      , iOSBundleSettings = IOS.Settings
        { IOS.addIconGloss = fromMaybe True
          $ Settings.iOSAddIconGloss settings

        , IOS.deviceFamilies = fromMaybe fullSet
          $ Settings.iOSDeviceFamilies settings

        , IOS.displayName = fromMaybe defaultDisplayName
          $ Settings.displayName settings

        -- The icons and app launch images in Info.plist are
        -- overridden by the build to reflect which assets
        -- are actually present.
        , IOS.icons = Set.empty
        , IOS.appLaunchImages = Set.empty

        , IOS.identifier = maybe defaultBundleID
          ApplicationID.toBundleID
          $ Settings.applicationID settings

        , IOS.orientations
          = setConcat . Set.map orientationToiOSOrientations
          . fromMaybe Settings.allOrientations
          $ Settings.orientations settings

        , IOS.runsInBackground = fromMaybe True
          $ Settings.iOSAppRunsInBackground settings

        , IOS.version = fromMaybe "1.0.0"
          $ Settings.version settings

        , IOS.urlSchemes = urlSchemes
        }

      , iOSResourcesPath = iOSResourcesPath'
      }

    , AndroidConfig
      { androidPackageName = maybe defaultPackageName
        ApplicationID.toPackageName
        $ Settings.applicationID settings
      , androidDisplayName = fromMaybe defaultDisplayName
        $ Settings.displayName settings
      , androidLoadingScreenPath = androidLoadingScreenPath'
      , androidResourcesPath = Settings.androidResourcesPath settings
      , androidOrientation = orientationsToAndroidOrientation
        . fromMaybe Settings.allOrientations
        $ Settings.orientations settings
      , androidReleaseMode = False
      , androidPlugins = mapMaybe Settings.parseAndroidPluginName
        [ "io.spaceport.plugins.contacts.ContactsPlugin"
        , "io.spaceport.plugins.mail.MailPlugin"
        , "io.spaceport.plugins.sms.SmsPlugin"
        ]
      , androidPermissions = Set.empty
      , androidSigningConfig = AndroidSigningConfig
        { androidKeystorePath = androidKeystorePath'
        , androidKeyAlias = fromMaybe "mykey"  -- See note [Keytool defaults].
          $ Settings.keyAlias settings
        , androidKeystorePassword = getPassword
          (Settings.keystorePasswordPath settings)
          "Android keystore password: "
          "Invalid Android keystore password."
        , androidKeyPassword = getPassword
          (Settings.keyPasswordPath settings)
          "Android key password: "
          "Invalid Android key password."
        }
      , androidVersion = fromMaybe "1.0.0"
        $ Settings.version settings
      , androidVersionCode = fromMaybe "100"
        $ Settings.versionCode settings
      , androidURLSchemes = urlSchemes
      }

    , Manifest.CDNConfig
      { Manifest.cdnConfigAuthorizationKey = unsafeUnMaybe
        "Missing Spaceport CDN authorization key (authorization_key)."
        $ Settings.authorizationKey settings
      , Manifest.cdnConfigPackageName
        = ApplicationID.toText
        . unsafeUnMaybe "Missing Spaceport CDN application ID (id)."
        $ Settings.applicationID settings
      }
    )

  where
    -- We use bang patterns here to force _|_ in case
    -- parsing fails.  (It's a program bug and should be
    -- caught ASAP.)
    !defaultBundleID = fromRight $ BundleID.fromText "untitled"
    !defaultPackageName = fromRight $ PackageName.fromText "io.spaceport.untitled"

    defaultDisplayName = "Untitled Game"
    urlSchemes = fromMaybe Set.empty $ Settings.urlSchemes settings
    maybeSetToList = maybe [] Set.toList
    getPassword maybePath prompt message = liftM Text.pack $ case maybePath of
      Just path -> readPasswordFromFile path
      Nothing -> do
        maybePass <- promptPassword prompt
          $ return . Right
        case maybePass of
          Nothing -> fail message
          Just pass -> return pass

-- Note [Keytool defaults]:
--
-- The "Option Defaults" section of the Keytool
-- documentation[1] specifies a default value of "mykey" for
-- the "-alias" option, and "$HOME/.keystore" for
-- "-keystore", where "$HOME" is the user's home directory.
--
-- [1]: http://docs.oracle.com/javase/1.4.2/docs/tooldocs/windows/keytool.html
