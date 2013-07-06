{-# LANGUAGE Rank2Types #-}

module Command
  ( Command(..)
  , parseCommand
  , execCommand
  ) where

import Control.Applicative
import Data.Foldable
import Data.Monoid
import Options.Applicative
import Options.Applicative.Builder.Internal (Mod, CommandFields)
import System.Exit

import qualified Data.Text as Text

import Command.Common
import ProgramInfo
import Sp

import qualified Command.Build
import qualified Command.Bundle
import qualified Command.Config
import qualified Command.Install
import qualified Command.Push
import qualified Command.Serve
import qualified Command.Simulator

newtype SubCommand = SubCommand (Sp ())

parseSubCommand :: Parser SubCommand
parseSubCommand = subparser $ mconcat
  [ subCommand Command.Build.parse     Command.Build.exec
  , subCommand Command.Bundle.parse    Command.Bundle.exec
  , subCommand Command.Config.parse    Command.Config.exec
  , subCommand Command.Install.parse   Command.Install.exec
  , subCommand Command.Push.parse      Command.Push.exec
  , subCommand Command.Serve.parse     Command.Serve.exec
  , subCommand Command.Simulator.parse Command.Simulator.exec
  ]

  where
    -- Converts a subcommand parser into a parser which
    -- yields an executing function, with the parsed
    -- configuration partially applied.
    subCommand
      :: CommandParser a
      -> CommandExecutor a
      -> Mod CommandFields SubCommand
    subCommand parser executor = parser $ fmap
      $ \ x -> SubCommand (executor x)

data Command
  = RunSubCommand Options SubCommand
  | ShowVersion

parseCommand :: Parser Command
parseCommand = asum
  [ pure ShowVersion
    <* flag' () (mconcat
      [ short 'V'
      , long "version"
      , help "Show version information."
      , hidden
      ])
  , RunSubCommand
    <$> parseOptions
    <*> parseSubCommand
  ]

execCommand :: Command -> IO ()
execCommand (RunSubCommand options (SubCommand exec))
  = runSp exec options
execCommand ShowVersion = do
  putStrLn $ "spaceport-push version: " ++ Text.unpack buildVersion
  putStrLn $ "Build date: " ++ show buildTimeStamp
  exitSuccess
