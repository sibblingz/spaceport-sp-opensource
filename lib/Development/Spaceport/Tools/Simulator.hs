{-# LANGUAGE RecordWildCards #-}

module Development.Spaceport.Tools.Simulator
  ( Simulator(..)
  , runSimulatorIO
  ) where

import Development.Spaceport.Common.Tools
import System.Process.Runner

data Simulator = Simulator
  { launchUrl :: Maybe String }

runSimulatorIO :: ExeRunner IO -> Runner IO Simulator
runSimulatorIO run Simulator {..}
  = run $ maybe [] (option "-u") launchUrl
