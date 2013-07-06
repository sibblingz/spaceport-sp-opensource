{-# LANGUAGE RecordWildCards #-}

module Command.Bundle
  ( Bundle(..)
  , BundleType(..)
  , BundleTarget(..)
  , parse
  , exec
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Monoid
import Data.Text (Text)
import Development.Shake.FilePath
import Options.Applicative
import System.Directory
import System.IO

import qualified Data.Text as Text
import qualified Development.Shake as Shake

import CheckBuild
import Command.Common
import Development.Spaceport.Android.Target
import Development.Spaceport.BuildConfig
import Development.Spaceport.BuildTarget
import Development.Spaceport.Core.Target
import Development.Spaceport.IOS.Target
import Development.Spaceport.Support
import Development.Spaceport.Util
import ModeConfig
import Sp
import System.IO.Prompt

import qualified Data.SpaceportSettings as Settings

data Bundle = Bundle
  { modeConfig :: ModeConfig
  , bundleTargets :: [BundleTarget]
  , description :: Maybe Text
  }

data BundleType = IPA | APK | CDNUpdate
  deriving (Eq, Ord)

instance Show BundleType where
  show IPA = "iOS IPA"
  show APK = "Android APK"
  show CDNUpdate = "Spaceport CDN bundle"

data BundleTarget = BundleTarget
  { targetType :: BundleType
  , targetPath :: FilePath
  } deriving (Eq)

instance Show BundleTarget where
  show BundleTarget {..}
    = targetPath ++ " (" ++ show targetType ++ ")"

pathBundleType :: FilePath -> Maybe BundleType
pathBundleType path
  | takeExtension path == ".ipa" = Just IPA
  | takeExtension path == ".apk" = Just APK
  | takeExtension path == ".zip" = Just CDNUpdate
  | otherwise = Nothing

readBundleTarget :: String -> Maybe BundleTarget
readBundleTarget path = BundleTarget
  <$> pathBundleType path
  <*> pure path

bundleTypeFile :: ModeConfig -> BundleType -> FilePath
bundleTypeFile ModeConfig {..} bundleType = case bundleType of
  IPA -> getPath $ IOSFile iOSMode "built.ipa"
  APK -> getPath $ AndroidFile androidMode "built.apk"
  CDNUpdate -> getPath $ GameZipFile gameZipMode
  where
    manifestMode = ManifestMode jsMode modeApplicationSource
    iOSMode = IOSMode manifestMode modeSpaceportBinary modeSigningMode
    androidMode = AndroidMode manifestMode modeSpaceportBinary modeSigningMode
    gameZipMode = GameZipMode $ ManifestMode jsMode CDN  -- Force CDN.
    jsMode = JSMode modeOptimizationLevel

parse :: CommandParser Bundle
parse f = command "bundle" . info parseArgs $ mconcat
  [ fullDesc
  , progDesc "Creates an iOS, Android, or Spaceport CDN bundle."
  ]
  where
    parseArgs = f $ Command.Bundle.Bundle
      <$> parseModeConfig
      <*> arguments1 readBundleTarget (mconcat
        [ help "Bundle target file. One of: .ipa .apk .zip"
        , metavar "FILE"
        ])
      <*> optional (nullOption $ mconcat
        [ short 'm'
        , long "description"
        , metavar "DESC"
        , help "Description of a Spaceport CDN bundle."
        , reader (Right . Text.pack)
        ])

-- | Copies to the bundle target from a build directory.
copyBundleTarget
  :: ModeConfig
  -> FilePath      -- ^ Build directory.
  -> BundleTarget  -- ^ Target to copy to.
  -> IO ()
copyBundleTarget modeConfig buildDirectory BundleTarget {..}
  = copyFile source targetPath
  where source = buildDirectory </> bundleTypeFile modeConfig targetType

exec :: CommandExecutor Command.Bundle.Bundle
exec Command.Bundle.Bundle {..} = do
  Tools {..} <- getToolsIO
  let hasTargetType t = any ((== t) . targetType) bundleTargets
  when (hasTargetType IPA)
    $ ensureiOSBuildable spSigner []
  when (hasTargetType APK) ensureAndroidBuildable

  -- CDN updates should have a description.
  (settings, warnings) <- if hasTargetType CDNUpdate
    then do
      desc <- liftIO $ maybe
        (Text.pack <$> prompt' "Describe this CDN bundle:")
        return description
      (settings, warnings) <- getSettings
      return (settings { Settings.appDescription = Just desc }, warnings)
    else getSettings

  liftIO $ mapM_ (hPrint stderr) warnings

  buildDir <- getOption buildDirectory
  runShake' settings . Shake.want $ map
    ((buildDir </>)
      . bundleTypeFile modeConfig
      . targetType)
    bundleTargets

  liftIO $ do
    mapM_ (copyBundleTarget modeConfig buildDir) bundleTargets
    putStrLn $ "Successfully bundled " ++ andList (map show bundleTargets) ++ "."
