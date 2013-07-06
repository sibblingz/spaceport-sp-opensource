{-# LANGUAGE OverloadedStrings #-}

module Development.Spaceport.Build
  ( allRules
  ) where

import Development.Shake

import Development.Spaceport.Android.Build
import Development.Spaceport.BuildConfig
import Development.Spaceport.Core.Build
import Development.Spaceport.IOS.Build
import Development.Spaceport.Support

import qualified Development.Spaceport.Manifest as Manifest

allRules
  :: ProjectConfig
  -> Support
  -> Tools Action
  -> IOSConfig
  -> AndroidConfig
  -> Manifest.CDNConfig
  -> FilePath
  -> Rules ()
allRules projectConfig support tools iOSConfig androidConfig cdnConfig outputPath = do
  gameRules projectConfig cdnConfig support tools outputPath
  iOSRules support tools iOSConfig outputPath
  androidRules support tools androidConfig outputPath
