{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Development.Spaceport.Android.Build
  ( androidRules
  ) where

import Data.Maybe
import Data.Set (Set)
import Data.Text (Text)
import Development.Shake
import Development.Shake.FilePath

import qualified Data.ByteString.Lazy as BSLazy
import qualified Data.Set as Set

import Development.ShakeExtras
import Development.Spaceport.Android.Target
import Development.Spaceport.Android.Manifest
import Development.Spaceport.Android.MyGame
import Development.Spaceport.BuildConfig
import Development.Spaceport.BuildTarget
import Development.Spaceport.Common.Tools
import Development.Spaceport.Core.Target
import Development.Spaceport.GameFile
import Development.Spaceport.RunTool
import Development.Spaceport.Support
import Development.Spaceport.Tools.Aapt
import Development.Spaceport.Tools.Dex
import Development.Spaceport.Tools.DexMerge
import Development.Spaceport.Tools.SpSigner
import Development.Spaceport.Tools.ZipAlign
import Development.Spaceport.Util

import qualified Codec.Archive.ZipExtras as Zip

keyDisplayName, keyKeyAlias, keyOrientation,
  keyPackageName, keyPlugins, keyURLSchemes,
  keyUserResources, keyVersion, keyVersionCode :: String
keyDisplayName   = androidKey "display name"
keyKeyAlias      = androidKey "key alias"
keyOrientation   = androidKey "orientation"
keyPackageName   = androidKey "package name"
keyPlugins       = androidKey "plugins"
keyURLSchemes    = androidKey "URL schemes"
keyUserResources = androidKey "user resources"
keyVersion       = androidKey "version"
keyVersionCode   = androidKey "version code"

androidKey :: String -> String
androidKey = ("Android " ++)

oracle :: AndroidConfig -> Rules ()
oracle AndroidConfig {..} = do
  keyPackageName   ~> androidPackageName
  keyDisplayName   ~> androidDisplayName
  keyVersion       ~> androidVersion
  keyVersionCode   ~> androidVersionCode
  keyOrientation   ~> androidOrientation
  keyPlugins       ~> androidPlugins
  keyKeyAlias      ~> androidKeyAlias androidSigningConfig
  keyUserResources ~> androidResourcesPath

  keyURLSchemes    ~>> Set.toList androidURLSchemes

androidRules
  :: Support
  -> Tools Action
  -> AndroidConfig
  -> FilePath
  -> Rules ()
androidRules support tools androidConfig outputPath = do
  oracle androidConfig
  buildRules support tools androidConfig outputPath

-- Constants taken from the Android SDK:
-- com.android.sdklib.internal.build.DebugKeyProvider
-- http://www.java2s.com/Open-Source/Android/android-core/platform-sdk/com/android/sdklib/internal/build/DebugKeyProvider.java.htm
debugKeyAlias, debugKeyDescription :: Text
debugKeyAlias = "AndroidDebugKey"
debugKeyDescription = "CN=Android Debug,O=Android,C=US"

debugKeyPassword, debugKeyStorePassword :: Password
debugKeyPassword = "android"
debugKeyStorePassword = "android"

debugKeyValidityDays :: Integer
debugKeyValidityDays = 40 * years
  where years = 365

-- TODO Remove dependency on AndroidConfig
buildRules
  :: Support
  -> Tools Action
  -> AndroidConfig
  -> FilePath
  -> Rules ()
buildRules Support {..} Tools {..} AndroidConfig {..} outputPath = do
  mapM_ rules permute
  debugKeyRules

  where
    obj p = outputPath </> getPath p

    debugKeyRules :: Rules ()
    debugKeyRules = obj AndroidDebugKeyStoreFile *> \ out -> do
      liftIO $ removeFileIfExists out
      runHidden_ "generate debug key store"
        $ spSigner GenerateKey
          { alias = debugKeyAlias
          , description = debugKeyDescription
          , validityDays = debugKeyValidityDays
          , keyPassword = debugKeyPassword
          , keyStoreFile = out
          , keyStorePassword = debugKeyStorePassword
          }

    rules :: AndroidMode -> Rules ()
    rules mode@(AndroidMode manifestMode spBinary signMode) = do
      built *> \ out -> do
        need [unaligned]
        runHidden_ "align zip archive" $ zipAlign ZipAlign
          { inputZip = unaligned
          , outputZip = out
          }

      unaligned *> \ out -> do
        need [unsigned]
        needKey keyKeyAlias
        keystorePath <- case androidKeystorePath of
          Nothing -> fail "Missing Android keystore file path."
          Just path -> return path
        need [keystorePath]
        keystorePassword <- liftIO androidKeystorePassword
        keyPassword <- liftIO androidKeyPassword
        runHidden_ "sign APK" $ spSigner SignJar
          { keystoreFile = keystorePath
          , keyAlias = androidKeyAlias
          , keystorePassword = keystorePassword
          , keyPassword = keyPassword
          , inputJar = unsigned
          , outputJar = out
          }

      unsigned *> \ out -> do
        need [classesDex, packaged, spSourceExtra]
        liftIO $ Zip.writeArchive out =<< Zip.buildArchive
          [ Zip.Input "" (Zip.ZipInput packaged)
          , Zip.Input "classes.dex" (Zip.FileInput classesDex)
          , Zip.Input "" (Zip.ZipInput spSourceExtra)
          ]

      packaged *> \ out -> do
        need
          [ androidJarPath
          , assets
          , androidManifest
          , allResources
          ]
        runHidden_ "package resources" $ aapt AaptPackageResources
          { manifestPath = androidManifest
          , resourcePath = allResources
          , includePaths = [androidJarPath]
          , assetsPaths = [assets]
          , releaseMode = buildRelease
          , packagePath = out
          }

      allResources *> \ out -> do
        let loadingScreenSGF = obj LoadingScreenFile
        need [requiredResourcesList, userResourcesList, loadingScreenSGF]
        requiredFiles <- readFileLines requiredResourcesList
        userFiles <- readFileLines userResourcesList

        liftIO $ removeDirectoryRecursiveIfExists out

        -- User files take absolute precedence.
        copyFiles requiredResources out requiredFiles
        copyFileWithDirectory loadingScreenSGF
          $ out </> "raw" </> "splashscreen.sgf"
        copyFiles userResources out userFiles

      requiredResourcesList *> fileListingRecursive requiredResources

      userResourcesList *> \ out -> do
        needKey keyUserResources
        fileListingRecursive userResources out

      assets *> \ out -> do
        gameFiles <- listGameFilesAction manifestMode outputPath
        copyGameFilesAction out gameFiles

      classesDex *> \ out
        -> runHidden_ "merge dex files" $ dexMerge DexMerge
          { firstDex = myGameDex
          , secondDex = androidSpaceportDex
          , mergedDex = out
          }

      myGameDex *> \ out
        -> runHidden_ "convert dex file" $ dex Dex
          { inputClass = myGameClass
          , outputDex = out
          }

      myGameClass *> \ out -> do
        needKey keyPackageName
        needLibrary
        liftIO . BSLazy.writeFile out $ mkMyGameClass manifestConfig

      androidManifest *> \ out -> do
        needKeys
          [ keyPackageName
          , keyOrientation
          , keyDisplayName
          , keyURLSchemes
          , keyVersion
          , keyVersionCode
          ]
        needLibrary
        writeFile' out $ mkAndroidManifestXml manifestConfig

      where
        manifestConfig = AndroidManifestXml
          { manifestPackageName = androidPackageName
          , manifestDisplayName = androidDisplayName
          , manifestVersionCode = androidVersionCode
          , manifestVersion = androidVersion
          , manifestOrientation = androidOrientation
          , manifestPermissions = allPermissions
          , manifestMenu = debugOrRelease True False
          , manifestURLSchemes = androidURLSchemes
          }

        debugOrRelease debug release = case spBinary of
          SpaceportDebug -> debug
          SpaceportRelease -> release

        buildRelease = debugOrRelease False True

        AndroidSigningConfig {..} = case signMode of
          DeveloperSigning -> AndroidSigningConfig
            { androidKeyAlias = debugKeyAlias
            , androidKeyPassword = return debugKeyPassword
            , androidKeystorePassword = return debugKeyStorePassword
            , androidKeystorePath = Just $ obj AndroidDebugKeyStoreFile
            }
          SubmissionSigning -> androidSigningConfig

        built     = androidObj "built.apk"
        packaged  = androidObj "packaged.ap_"
        unaligned = androidObj "unaligned.apk"
        unsigned  = androidObj "unsigned.apk"

        androidManifest = androidObj "AndroidManifest.xml"
        assets          = androidObj "assets"
        classesDex      = androidObj "classes.dex"

        allResources = androidObj "res"

        spSourceExtra = debugOrRelease
          androidDebugExtraPath androidReleaseExtraPath

        userResources = fromMaybe androidDefaultResourcesPath
          androidResourcesPath
        userResourcesList = androidObj "user-res.lst"

        requiredResources = androidRequiredResourcesPath
        requiredResourcesList = androidObj "required-res.lst"

        myGameClass = androidObj "MyGame.class"
        myGameDex = androidObj "MyGame.dex"

        allPermissions
          = androidPermissions `Set.union` modePermissions mode

        androidObj = obj . AndroidFile mode

-- | Permissions absolutely required by all Spaceport
-- Android builds.
--
-- FIXME This list is excessive.
corePermissions :: Set Permission
corePermissions = Set.fromList
  [ Internet
  , AccessNetworkState
  , AccessWiFiState
  , ReadInputState
  , WriteExternalStorage
  , Vibrate
  , ModifyAudioSettings
  , ReadContacts
  , CloudToDeviceMessaging
  , CloudToDeviceReceive
  , GetAccounts
  , WakeLock
  ]

-- | Permissions required for a specific Android build
-- configuration.
modePermissions :: AndroidMode -> Set Permission
modePermissions (AndroidMode _ SpaceportRelease _)
  -- Spaceport release builds don't use multicast.
  = corePermissions `Set.union` Set.singleton ChangeWiFiMulticastState
modePermissions _ = corePermissions
