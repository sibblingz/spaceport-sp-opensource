module Main
  ( main
  ) where

import Test.Framework (defaultMain)

import qualified Test.Data.Android.PackageName
import qualified Test.Data.IOS.BundleID

import qualified Test.Data.Plist.Decode
import qualified Test.Data.Plist.EncodeDecode

import qualified Test.Data.SpaceportSettings

import qualified Test.Data.Text.Edit

import qualified Test.Development.ActionScript3.Project
import qualified Test.Development.Spaceport.Manifest
import qualified Test.Development.Spaceport.Util

import qualified Test.Text.JavaProperties
import qualified Test.Text.JavaProperties.Parse

main :: IO ()
main = defaultMain $ concat
  [ Test.Data.Android.PackageName.tests
  , Test.Data.IOS.BundleID.tests

  , Test.Data.Plist.Decode.tests
  , Test.Data.Plist.EncodeDecode.tests

  , Test.Data.SpaceportSettings.tests

  , Test.Data.Text.Edit.tests

  , Test.Development.ActionScript3.Project.tests
  , Test.Development.Spaceport.Manifest.tests
  , Test.Development.Spaceport.Util.tests

  , Test.Text.JavaProperties.tests
  , Test.Text.JavaProperties.Parse.tests
  ]
