{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}

module Command.Common
  ( CommandParser
  , CommandExecutor

  , parseOptions
  , runShake
  , runShake'
  ) where

import Control.Applicative
import Control.Monad.IO.Class
import Data.Monoid
import Data.Traversable
import Development.Shake.FilePath
import Options.Applicative
import Options.Applicative.Builder.Internal (Mod, CommandFields)
import System.IO

import qualified Data.Text as Text
import qualified Development.Shake as Shake

import Development.Spaceport.Build
import Development.Spaceport.BuildConfig
import Development.Spaceport.Support
import Development.Spaceport.Util
import Sp

import qualified Data.SpaceportSettings as Settings

type CommandParser a
  = forall b. (Parser a -> Parser b)  -- fmap
  -> Mod CommandFields b

type CommandExecutor a
  = a -> Sp ()

defaultOutputDirectory :: FilePath
defaultOutputDirectory = "out"  -- Modest, yet successful.

readJobCount :: String -> Either ParseError Int
readJobCount s = case maybeRead s of
  Just jobCount
    | jobCount >= 1 -> Right jobCount
    | otherwise -> Left . ErrorMsg
      $ "Invalid job count '" ++ show jobCount
      ++ "': at least one job thread is required."
  Nothing -> Left . ErrorMsg
    $ "Invalid job count '" ++ s ++ "'."

orDefault :: (Alternative f) => f a -> a -> f a
f `orDefault` x = f <|> pure x

parseOptions :: Parser Options
parseOptions = Options
  <$> switch (mconcat
    [ short 'v'
    , long "verbose"
    , help "Print debugging information during execution."
    , internal
    ])
  <*> nullOption (mconcat
    [ short 'j'
    , long "jobs"
    , metavar "JOBS"
    , help "Number of threads to execute simultaneously."
    , reader readJobCount
    , hidden
    ]) `orDefault` 2
  <*> switch (mconcat
    [ short 'k'
    , long "keep-going"
    , help "Report as many problems as possible before terminating."
    , hidden
    ])
  <*> strOption (mconcat
    [ short 'o'
    , long "output"
    , metavar "DIR"
    , help "Directory for build artifacts."
    , internal
    ]) `orDefault` defaultOutputDirectory
  <*> switch (mconcat
    [ long "ignore-missing-project-file"
    , help "Don't error if a project file is missing."
    , internal
    ])
  <*> optional (strOption $ mconcat
    [ short 'f'
    , long "project-file"
    , metavar "FILE"
    , help "AS3 project file to load."
    ])
  <*> mconcatParsers settingsParsers

  where
    settingsParsers = map settingParsers Settings.allSettingDescs
    settingParsers Settings.SettingDesc {..}
      = (dupable . nullOption $ mconcat
        [ long $ "set:" ++ Text.unpack keyName
        , help $ Text.unpack description
        , reader $ either (Left . ErrorMsg) Right
          . parseValue . Text.pack
        , internal
        ]) `orDefault` mempty

dupable :: (Monoid a) => Parser a -> Parser a
dupable = fmap mconcat . some

mconcatParsers :: (Monoid a) => [Parser a] -> Parser a
mconcatParsers = fmap mconcat . sequenceA

runShake :: Shake.Rules () -> Sp ()
runShake rules = do
  (settings, warnings) <- getSettings
  liftIO $ mapM_ (hPrint stderr) warnings
  runShake' settings rules

runShake' :: Settings.Settings -> Shake.Rules () -> Sp ()
runShake' settings rules = do
  toolsIO <- getToolsIO
  (projectConfig, iOSConfig, androidConfig, cdnConfig)
    <- liftIO $ fromSettings (spSigner toolsIO) settings

  Options {..} <- getOptions
  support <- getSupport
  tools <- getToolsAction

  let
    shakeOptions = Shake.shakeOptions
      { Shake.shakeVerbosity = if verbose then Shake.Loud else Shake.Quiet
      , Shake.shakeFiles = buildDirectory </> "build-cache"
      , Shake.shakeThreads = parallelJobs
      , Shake.shakeStaunch = keepGoing
      }
  liftIO . Shake.shake shakeOptions $ do
    allRules projectConfig support tools
      iOSConfig androidConfig cdnConfig
      buildDirectory
    rules
