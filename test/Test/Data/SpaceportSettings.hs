{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Data.SpaceportSettings where

import Data.Text (Text)
import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit
import Test.Framework.TH
import Test.HUnit

import Data.SpaceportSettings
import Development.Spaceport.Util (fromRight)

import QuasiQuoters

import qualified Data.ApplicationID as ApplicationID

main = defaultMain tests
tests = $(testGroupGenerator)
  : hUnitTestToTests (TestLabel "simpleSettings" tests_simpleSettings)
  ++ hUnitTestToTests (TestLabel "defaultSettings" tests_defaultSettings)

case_emptySettings = settings @?= empty
  where Right settings = parseText $ defaultSettingsText CommentKeys

appID :: Text -> ApplicationID.ApplicationID
appID = fromRight . ApplicationID.fromText

tests_defaultSettings = TestList
  [ applicationID s ~?= Just (appID "io.spaceport.untitled")
  , version s ~?= Just "1.0.0"
  , displayName s ~?= Just "Untitled Game"
  , keyAlias s ~?= Just "mykey"
  ]
  where Right s = parseText $ defaultSettingsText IncludeKeys

tests_simpleSettings = TestList
  [ iOSDevMobileProvisionFile s ~?= Just "/Users/mg/Documents/iOS Bullshit/Matthew_Glazar_Sandbox.mobileprovision"
  , iOSDevIdentity s ~?= Just "/Users/mg/Documents/iOS Bullshit/Certificates.p12"
  , applicationID s ~?= Just (appID "io.spaceport.liranslab")
  , displayName s ~?= Just "Liran's Lab"
  ]
  where
    Right s = parseText [text|
display_name = Liran's Lab
orientations = landscape
ios_resources = resources/
loading_screen_Filek = src/content/loadingscreen.swf

id = io.spaceport.liranslab
authorization_key = supersikritkey

ios_dev_mobile_provision_file = /Users/mg/Documents/iOS Bullshit/Matthew_Glazar_Sandbox.mobileprovision
ios_dev_identity = /Users/mg/Documents/iOS Bullshit/Certificates.p12
|]
