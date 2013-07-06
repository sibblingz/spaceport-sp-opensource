{-# LANGUAGE RecordWildCards #-}

module Command.Build
  ( Build(..)
  , BuildTarget(..)
  , parse
  , exec
  ) where

import Control.Applicative
import Control.Monad
import Data.Monoid
import Development.Shake.FilePath
import Options.Applicative

import qualified Development.Shake as Shake

import CheckBuild
import Command.Common
import Development.Spaceport.Android.Target
import Development.Spaceport.BuildConfig
import Development.Spaceport.BuildTarget
import Development.Spaceport.Core.Target
import Development.Spaceport.IOS.Target
import Development.Spaceport.Support
import ModeConfig
import Sp

data Build = Build
  { buildTargets :: [BuildTarget]
  , modeConfig :: ModeConfig
  }

data BuildTarget = Game | IPA | APK | CDNUpdate
  deriving (Bounded, Eq, Enum)

instance Show BuildTarget where
  show Game = "game"
  show IPA = "ipa"
  show APK = "apk"
  show CDNUpdate = "cdn"

readBuildTarget :: String -> Maybe BuildTarget
readBuildTarget "game" = Just Game
readBuildTarget "ipa" = Just IPA
readBuildTarget "apk" = Just APK
readBuildTarget "cdn" = Just CDNUpdate
readBuildTarget _ = Nothing

parse :: CommandParser Build
parse f = command "build" . info parseArgs $ mconcat
  [ fullDesc
  , progDesc "Builds a project, reporting problems with the project."
  ]
  where
    parseArgs = f $ Build
      <$> arguments readBuildTarget (mconcat
        [ help $ "Build target.  One of: "
          ++ unwords (map show [minBound .. maxBound :: BuildTarget])
        ])
      <*> parseModeConfig

buildTargetFiles :: ModeConfig -> BuildTarget -> [FilePath]
buildTargetFiles ModeConfig {..} target = case target of
  Game -> [getPath $ ManifestFile manifestMode]
  IPA -> [getPath $ IOSFile iOSMode "built.ipa"]
  APK -> [getPath $ AndroidFile androidMode "built.apk"]
  CDNUpdate -> [getPath $ GameZipFile gameZipMode]
  where
    manifestMode = ManifestMode jsMode modeApplicationSource
    iOSMode = IOSMode manifestMode modeSpaceportBinary modeSigningMode
    androidMode = AndroidMode manifestMode modeSpaceportBinary modeSigningMode
    gameZipMode = GameZipMode $ ManifestMode jsMode CDN  -- Force CDN.
    jsMode = JSMode modeOptimizationLevel

exec :: CommandExecutor Build
exec Build {..} = do
  Tools {..} <- getToolsIO
  when (IPA `elem` buildTargets)
    $ ensureiOSBuildable spSigner []
  when (APK `elem` buildTargets) ensureAndroidBuildable

  buildDir <- getOption buildDirectory
  runShake . Shake.want
    . map (buildDir </>)
    . Prelude.concatMap (buildTargetFiles modeConfig)
    $ case buildTargets of
      [] -> [Game]
      _ -> buildTargets
