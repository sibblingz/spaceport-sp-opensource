{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Development.Spaceport.IOS.Build
  ( iOSRules
  ) where

import Control.Monad
import Data.Maybe
import Development.Shake as Shake
import Development.Shake.FilePath
import System.Directory
import System.IO.Temp

import qualified Data.Set as Set

import Development.ShakeExtras
import Development.Spaceport.BuildConfig
import Development.Spaceport.BuildTarget
import Development.Spaceport.Common.Tools
import Development.Spaceport.Core.Target
import Development.Spaceport.IOS.Target
import Development.Spaceport.IOS.Tools
import Development.Spaceport.RunTool
import Development.Spaceport.Support
import Development.Spaceport.Tools.SpSigner
import Development.Spaceport.Util

import qualified Data.IOS.InfoPlist as InfoPlist

keyBundleSettings, keyCertProvision, keyResourcesPath :: String
keyBundleSettings = iOSKey "bundle settings"
keyCertProvision  = iOSKey "provisioning profile and certificate"
keyResourcesPath  = iOSKey "resources path"

iOSKey :: String -> String
iOSKey = ("iOS " ++)

oracle :: IOSConfig -> Rules ()
oracle IOSConfig {..} = do
  keyBundleSettings ~> iOSBundleSettings
  keyResourcesPath ~> iOSResourcesPath
  keyCertProvision ~>>
    [ show iOSMobileProvisionFile
    , show iOSIdentity
    ]

iOSRules
  :: Support
  -> Tools Action
  -> IOSConfig
  -> FilePath
  -> Rules ()
iOSRules support tools iOSConfig outputPath = do
  oracle iOSConfig
  buildRules support tools iOSConfig outputPath

-- TODO Remove dependency on IOSConfig
buildRules
  :: Support
  -> Tools Action
  -> IOSConfig
  -> FilePath
  -> Rules ()
buildRules Support {..} Tools {..} IOSConfig {..} outputPath
  = mapM_ rules permute

  where
    obj p = outputPath </> getPath p

    rules :: IOSMode -> Rules ()
    rules mode@(IOSMode manifestMode spBinary _signMode) = do
      iconResourcesList *> \ out -> do
        needKey keyResourcesPath
        fileListingPattern resourcesPath "Icon*.png" out

      splashScreenResourcesList *> \ out -> do
        needKey keyResourcesPath
        fileListingPattern resourcesPath "Default*.png" out

      obj (IOSFile mode "splashscreen.sgf")
        *> copyFile' (obj LoadingScreenFile)

      obj (IOSFile mode "built.ipa") *> \ out -> do
        need [appBundle]
        ipaFromAppBundle appBundle out

      appBundle *> \ out -> do
        needKeys
          [ keyBundleSettings
          , keyCertProvision
          , keyResourcesPath
          ]
        need
          [ obj LoadingScreenFile
          , manifestFile
          , iconResourcesList
          , splashScreenResourcesList
          ]
        needLibrary

        liftIO $ removeDirectoryRecursiveIfExists out

        outTemp <- liftIO $ createTempDirectory
          (dropFileName out)
          (takeFileName out <.> "tmp")

        icons <- readFileLines iconResourcesList
        splashScreens <- readFileLines splashScreenResourcesList

        let
          bundleSettings = iOSBundleSettings
            { InfoPlist.icons = Set.fromList icons
            , InfoPlist.appLaunchImages = Set.fromList splashScreens
            }

        -- Create the basics.
        gameFiles <- listGameFilesAction manifestMode outputPath
        createUnsignedAppBundle
          templateAppBundle gameFiles bundleSettings outTemp

        -- Copy resources.
        let resources = icons ++ splashScreens
        need $ map (resourcesPath </>) resources
        forM_ resources $ \ path -> copyFile'
          (resourcesPath </> path)
          (outTemp </> path)

        -- Copy loading screen (which is called
        -- splashscreen.sgf for some reason).
        copyFile'
          (obj LoadingScreenFile)
          (outTemp </> "splashscreen.sgf")

        -- Sign the bundle.
        mobileProvisionFile <- maybe
          (fail "Missing mobile provision profile file path") return
          iOSMobileProvisionFile
        identity <- maybe
          (fail "Missing signing identity") return
          iOSIdentity
        runHidden_ "sign iOS application" $ spSigner SignAppBundle
          { mobileProvisionFile = mobileProvisionFile
          , identity = identity
          , appBundle = outTemp
          }

        liftIO $ renameDirectory outTemp out

      where
        templateAppBundle = case spBinary of
          SpaceportDebug -> unsignedSpaceportiOSBundleDebug
          SpaceportRelease -> unsignedSpaceportiOSBundleRelease

        manifestFile = obj $ ManifestFile manifestMode
        appBundle = obj $ IOSFile mode "built.app"
        iconResourcesList = obj $ IOSFile mode "icon_resources.lst"
        splashScreenResourcesList = obj $ IOSFile mode "splash_resources.lst"

        resourcesPath = fromMaybe iOSDefaultResourcesPath iOSResourcesPath
