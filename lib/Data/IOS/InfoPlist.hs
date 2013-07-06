{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.IOS.InfoPlist
  ( Orientation(..)
  , DeviceFamily(..)
  , Settings(..)

  , applySettings
  , deviceFamilyID
  ) where

import Data.IOS.BundleID (BundleID)
import Data.Monoid
import Data.Set (Set)
import Data.Text (Text)

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified Data.IOS.BundleID as BundleID
import qualified Data.Plist as Plist

data Orientation
  = LandscapeLeft
  | LandscapeRight
  | PortraitUpsideDown
  | Portrait
  deriving (Eq, Ord, Show, Enum, Bounded)

data DeviceFamily
  = IPhone | IPad
  deriving (Eq, Ord, Show, Enum, Bounded)

defaultOrientation :: DeviceFamily -> Set Orientation -> Orientation
defaultOrientation _ orientations
  | Portrait `Set.member` orientations = Portrait
  | PortraitUpsideDown `Set.member` orientations = PortraitUpsideDown
  | LandscapeRight `Set.member` orientations = LandscapeRight
  | LandscapeLeft `Set.member` orientations = LandscapeLeft
  | otherwise = Portrait

-- | The ID of a device family as used by the UIDeviceFamily
-- Info.plist key.
--
-- https://developer.apple.com/library/ios/#documentation/General/Reference/InfoPlistKeyReference/Articles/iPhoneOSKeys.html#//apple_ref/doc/uid/TP40009252-SW11
deviceFamilyID :: DeviceFamily -> Int
deviceFamilyID IPhone = 1
deviceFamilyID IPad = 2

data Settings = Settings
  { addIconGloss :: Bool
  , appLaunchImages :: Set FilePath
  , deviceFamilies :: Set DeviceFamily
  , displayName :: Text
  , icons :: Set FilePath
  , identifier :: BundleID
  , orientations :: Set Orientation
  , runsInBackground :: Bool
  , version :: Text
  , urlSchemes :: Set Text
  } deriving (Show)

applySettings
  :: Settings
  -> Plist.Value
  -> Either String Plist.Value
applySettings Settings {..} (Plist.Dictionary kvps)
  = Right . Plist.Dictionary $ Map.fromList
  [ ("CFBundleDisplayName", str displayName)
  , ("CFBundleIdentifier", str identifierText)
  , ("CFBundleVersion", str version)
  , ("UIApplicationExitsOnSuspend", bool $ not runsInBackground)
  , ("UIPrerenderedIcon", bool $ not addIconGloss)
  , ( "CFBundleIconFiles"
    , Plist.Array . map (str . Text.pack) $ Set.toList icons
    )
  , ( "UIDeviceFamily"
    , Plist.Array . map (Plist.Integer . fromIntegral . deviceFamilyID)
      $ Set.toList deviceFamilies
    )
  , ( "UISupportedInterfaceOrientations", orientationsArray IPhone)
  , ( "UISupportedInterfaceOrientations~ipad", orientationsArray IPad)
  , ( "UIInterfaceOrientation"
    , orientationString $ defaultOrientation IPhone orientations
    )
  , ( "UIInterfaceOrientation~ipad"
    , orientationString $ defaultOrientation IPad orientations
    )
  ]
  <> urlSchemesEntry
  <> kvps

  where
    identifierText = BundleID.toText identifier

    orientationsArray family
      = Plist.Array . map orientationString
      $ orientationsList family

    -- The default orientation must be first, otherwise
    -- UIInterfaceOrientation does not work properly.
    orientationsList family
      = defaultOrientation family orientations
      : Set.toList orientations

    str = Plist.String
    bool = Plist.Boolean

    orientationString o = str $ "UIInterfaceOrientation" <> case o of
      LandscapeLeft      -> "LandscapeLeft"
      LandscapeRight     -> "LandscapeRight"
      PortraitUpsideDown -> "PortraitUpsideDown"
      Portrait           -> "Portrait"

    urlSchemesEntry
      | Set.null urlSchemes = mempty
      | otherwise = Map.singleton "CFBundleURLTypes" . Plist.Array
        . map urlSchemeEntry $ Set.toList urlSchemes

    urlSchemeEntry scheme = Plist.Dictionary $ Map.fromList
      [ ("CFBundleURLName", str $ identifierText <> "." <> scheme)
      , ("CFBundleURLSchemes", Plist.Array [str scheme])
      ]

applySettings _ _ = Left "Invalid plist file"

