{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Development.Spaceport.Android.Manifest
  ( AndroidManifestXml(..)
  , Orientation(..)
  , Password
  , Permission(..)
  , mkAndroidManifestXml
  ) where

import Data.Android.PackageName (PackageName)
import Data.Set (Set)
import Data.Text (Text)

import qualified Data.Android.PackageName as PackageName
import qualified Data.Set as Set
import qualified Data.Text as Text

type Password = Text

data Orientation
  = Landscape
  | Portrait
  | FullSensor
  deriving (Eq, Ord, Show, Enum, Bounded)

data Permission
  = Internet
  | AccessNetworkState
  | AccessWiFiState
  | ChangeWiFiMulticastState
  | ReadInputState
  | WriteExternalStorage
  | Vibrate
  | ModifyAudioSettings
  | ReadContacts
  | CloudToDeviceMessaging
  | CloudToDeviceReceive
  | GetAccounts
  | WakeLock
  deriving (Eq, Ord, Show, Enum, Bounded)

-- Note [Android orientations]:
--
-- According to the Android documentation[1], the "portrait"
-- and "landscape" orientation specifiers refer to fixed
-- orientations, that is, the application will not be
-- reoriented if the device is rotated one half-turn. The
-- alternatives are "sensorPortrait" and "sensorLandscape",
-- which were introduced in API level 9 (2.3.x).
-- Unfortunately, even when the SDK version attributes are
-- set accordingly, the APK fails to build:
--
--     String types not allowed at 'screenOrientation'
--     with value 'sensorPortrait'
--
-- [1]: http://developer.android.com/guide/topics/manifest/activity-element.html#screen

data AndroidManifestXml = AndroidManifestXml
  { manifestPackageName :: PackageName  -- ^ Package of entry point.
  , manifestDisplayName :: Text  -- ^ Application display name.
  , manifestVersionCode :: Text  -- ^ Android internal version code.
  , manifestVersion     :: Text  -- ^ Version string.
  , manifestOrientation :: Orientation
  , manifestPermissions :: Set Permission
  , manifestMenu        :: Bool  -- ^ If true, include settings menu.
  , manifestURLSchemes  :: Set Text
  }

mkAndroidManifestXml :: AndroidManifestXml -> String
mkAndroidManifestXml AndroidManifestXml {..} = Text.unpack $ Text.concat
  [ "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n\
  \<manifest xmlns:android=\"http://schemas.android.com/apk/res/android\"\n\
  \  package=\"", escapeXML (PackageName.toText manifestPackageName), "\"\n\
  \  android:versionCode=\"", escapeXML manifestVersionCode, "\"\n\
  \  android:versionName=\"", escapeXML manifestVersion, "\">\n\
  \    <uses-sdk\n\
  \      android:targetSdkVersion=\"10\"\n\
  \      android:minSdkVersion=\"10\"/>\n\
  \    <uses-feature android:name=\"android.hardware.screen.landscape\"/>\n\
  \    <uses-feature android:name=\"android.hardware.screen.portrait\"/>\n\
  \    <!-- OpenGL ES 2.0 -->\n\
  \    <uses-feature\n\
  \      android:required=\"true\"\n\
  \      android:glEsVersion=\"0x00020000\"/>\n\
  \    <supports-screens\n\
  \      android:anyDensity=\"true\"\n\
  \      android:largeScreens=\"true\"\n\
  \      android:smallScreens=\"true\"\n\
  \      android:resizeable=\"true\"\n\
  \      android:normalScreens=\"true\"/>\n"
  , Text.unlines $ map permissionXMLText (Set.toList manifestPermissions)
  , "\n\
  \    <application\n\
  \      android:label=\"", escapeXML manifestDisplayName, "\"\n\
  \      android:icon=\"@drawable/sp_icon\">\n\
  \        <activity\n\
  \          android:icon=\"@drawable/sp_icon\"\n\
  \          android:theme=\"@android:style/Theme.NoTitleBar.Fullscreen\"\n\
  \          android:clearTaskOnLaunch=\"true\"\n\
  \          android:launchMode=\"singleTask\"\n\
  \          android:screenOrientation=\"", orientationString, "\"\n\
  \          android:configChanges=\"orientation|keyboard|keyboardHidden\
                                    \|locale|fontScale|navigation|mcc|mnc\
                                    \|touchscreen\"\n\
  \          android:label=\"", escapeXML manifestDisplayName, "\"\n\
  \          android:name=\".MyGame\">\n\
  \            <intent-filter>\n\
  \                <action android:name=\"android.intent.action.MAIN\"/>\n\
  \                <category android:name=\"android.intent.category.LAUNCHER\"/>\n\
  \            </intent-filter>\n"
  , urlSchemeEntries
  , "        </activity>\n"
  , if manifestMenu then
  "        <activity android:icon=\"@drawable/sp_icon_settings\"\n\
  \            android:theme=\"@android:style/Theme.Dialog\"\n\
  \            android:configChanges=\"orientation|keyboard|keyboardHidden\
                                      \|locale|fontScale|navigation|mcc|mnc\
                                      \|touchscreen\"\n\
  \            android:label=\"@string/preference_activity_label\"\n\
  \            android:name=\"sibblingz.spaceport.SpaceportAppPreferences\"\n\
  \            android:taskAffinity=\"sibblingz.spaceport.SpaceportAppPreferences\">\n\
  \        </activity>\n"
    else ""
  , "\n\
  \        <receiver android:name=\"com.google.android.gcm.GCMBroadcastReceiver\" android:permission=\"com.google.android.c2dm.permission.SEND\">\n\
  \            <intent-filter>\n\
  \                <action android:name=\"com.google.android.c2dm.intent.RECEIVE\"/>\n\
  \                <action android:name=\"com.google.android.c2dm.intent.REGISTRATION\"/>\n\
  \                <category android:name=\"sibblingz.spaceport\"/>\n\
  \            </intent-filter>\n\
  \        </receiver>\n\
  \        <service android:name=\".GCMIntentService\"/>\n\
  \    </application>\n\
  \</manifest>\n"
  ]
  where
    orientationString = case manifestOrientation of
      Landscape  -> "landscape"  -- See Note [Android orientations]
      Portrait   -> "portrait"
      FullSensor -> "fullSensor"

    permissionXMLText permission = Text.concat
      [ "<uses-permission android:name=\""
      , escapeXML $ permissionFQN permission
      , "\"/>"
      ]

    permissionFQN permission = case permission of
      Internet                 -> "android.permission.INTERNET"
      AccessNetworkState       -> "android.permission.ACCESS_NETWORK_STATE"
      AccessWiFiState          -> "android.permission.ACCESS_WIFI_STATE"
      ChangeWiFiMulticastState -> "android.permission.CHANGE_WIFI_MULTICAST_STATE"
      ReadInputState           -> "android.permission.READ_INPUT_STATE"
      WriteExternalStorage     -> "android.permission.WRITE_EXTERNAL_STORAGE"
      Vibrate                  -> "android.permission.VIBRATE"
      ModifyAudioSettings      -> "android.permission.MODIFY_AUDIO_SETTINGS"
      ReadContacts             -> "android.permission.READ_CONTACTS"
      CloudToDeviceMessaging   -> "sibblingz.spaceportapp.permission.C2D_MESSAGE"
      CloudToDeviceReceive     -> "com.google.android.c2dm.permission.RECEIVE"
      GetAccounts              -> "android.permission.GET_ACCOUNTS"
      WakeLock                 -> "android.permission.WAKE_LOCK"

    urlSchemeEntries = Text.concat . map urlSchemeEntry
      $ Set.toList manifestURLSchemes
    urlSchemeEntry scheme = Text.concat
      [ "<intent-filter>\n\
        \    <action android:name=\"android.intent.action.VIEW\"/>\n\
        \    <category android:name=\"android.intent.category.DEFAULT\"/>\n\
        \    <category android:name=\"android.intent.category.BROWSABLE\"/>\n\
        \    <data android:scheme=\"", escapeXML scheme, "\"/>\n\
        \</intent-filter>\n"
      ]

escapeXML :: Text -> Text
escapeXML = id
