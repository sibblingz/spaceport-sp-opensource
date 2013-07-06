module ModeConfig
  ( debugModeConfig
  , debugCDNModeConfig
  , submissionModeConfig
  , releaseModeConfig
  , parseModeConfig
  ) where

import Control.Applicative
import Data.Foldable
import Data.Monoid
import Options.Applicative

import Development.Spaceport.BuildConfig
import Development.Spaceport.BuildTarget

debugModeConfig, debugCDNModeConfig, submissionModeConfig, releaseModeConfig
  :: ModeConfig

debugModeConfig
  = ModeConfig Unoptimized Bundle SpaceportDebug DeveloperSigning
debugCDNModeConfig
  = ModeConfig Unoptimized CDN SpaceportDebug DeveloperSigning
submissionModeConfig
  = ModeConfig Optimized CDN SpaceportRelease SubmissionSigning
releaseModeConfig
  = ModeConfig Optimized CDN SpaceportRelease DeveloperSigning

parseModeConfig :: Parser ModeConfig
parseModeConfig = asum
  [ configFlag debugModeConfig "debug"
    "Builds a WiFi-enabled development application."
  , configFlag debugCDNModeConfig "debug-cdn"
    "Builds a CDN-enabled development application."
  , configFlag submissionModeConfig "submission"
    "Builds a CDN-enabled application for submission to Apple or Google."
  , configFlag releaseModeConfig "release"
    "Builds a CDN-enabled application."
  , pure debugModeConfig
  ]
  where
    configFlag config name description = flag' config
      $ mconcat [long name, help description, hidden]
