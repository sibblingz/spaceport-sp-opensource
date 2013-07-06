{-# LANGUAGE RecordWildCards #-}

module Command.Simulator
  ( Simulator(..)
  , parse
  , exec
  ) where

import Control.Applicative
import Control.Monad.IO.Class
import Data.Monoid
import Development.Shake.FilePath
import Options.Applicative

import qualified Development.Shake as Shake

import Command.Common
import Development.Spaceport.BuildConfig
import Development.Spaceport.BuildTarget
import Development.Spaceport.Core.Target
import Development.Spaceport.GameFile
import ModeConfig
import Server
import Simulator
import Sp

data Simulator = Simulator
  { modeConfig :: ModeConfig }

parse :: CommandParser Simulator
parse f = command "sim" . info parseArgs $ mconcat
  [ fullDesc
  , progDesc "Runs a project in the Spaceport Simulator."
  ]
  where
    parseArgs = f . pure $ Simulator debugModeConfig

exec :: CommandExecutor Simulator
exec Simulator {..} = do
  buildDir <- getOption buildDirectory
  let gameFileListPath = buildDir
        </> getPath (GameFilesFile manifestMode)
  runShake $ Shake.want [gameFileListPath]

  tools <- getToolsIO
  liftIO $ do
    gameFiles <- readGameFileListFile gameFileListPath
    (_threadID, port) <- startStaticServer buildDir
      $ map (makeGameFileRelative buildDir) gameFiles
    launchSimulator tools port

  where
    manifestMode = ManifestMode jsMode modeApplicationSource
    jsMode = JSMode modeOptimizationLevel
    ModeConfig {..} = modeConfig
