{-# LANGUAGE RecordWildCards #-}

module Command.Serve
  ( Serve(..)
  , parse
  , exec
  ) where

import Control.Applicative
import Control.Monad.IO.Class
import Data.Monoid
import Data.Word
import Development.Shake.FilePath
import Options.Applicative

import qualified Development.Shake as Shake
import qualified Network.Socket as Net

import Command.Common
import Development.Spaceport.BuildConfig
import Development.Spaceport.BuildTarget
import Development.Spaceport.Core.Target
import Development.Spaceport.GameFile
import Development.Spaceport.Util
import ModeConfig
import Server
import Sp

data Serve = Serve
  { modeConfig :: ModeConfig
  , portNumber :: Net.PortNumber
  }

readPortNumber :: String -> Either ParseError Net.PortNumber
readPortNumber s = case maybeRead s of
  Just n
    | validPort n -> Right $ fromIntegral (n :: Word16)
    | otherwise -> Left $ ErrorMsg "Not a valid port"
  Nothing -> Left $ ErrorMsg "Not a number"
  where
    validPort n = portMin <= n && n <= portMax
    portMin = 1
    portMax = 65535

parse :: CommandParser Serve
parse f = command "serve" . info parseArgs $ mconcat
  [ fullDesc
  , progDesc "Host a Spaceport application on an HTTP server."
  ]
  where
    parseArgs = f $ Serve
      <$> parseModeConfig
      <*> nullOption (mconcat
        [ short 'p'
        , long "port"
        , metavar "PORT"
        , help "TCP port to host the server on."
        , reader readPortNumber
        ])

exec :: CommandExecutor Serve
exec Serve {..} = do
  buildDir <- getOption buildDirectory
  let gameFileListPath = buildDir
        </> getPath (GameFilesFile manifestMode)
  gameFiles <- do
    runShake $ Shake.want [gameFileListPath]
    rawGameFiles <- liftIO $ readGameFileListFile gameFileListPath
    return $ map (mapLocalPath (buildDir </>)) rawGameFiles

  liftIO $ do
    (_socket, runServer) <- prepareStaticServer [portNumber] buildDir
      $ map (makeGameFileRelative buildDir) gameFiles

    putStrLn $ "Hosting application on port " ++ show portNumber
    runServer

  where
    manifestMode = ManifestMode jsMode modeApplicationSource
    jsMode = JSMode modeOptimizationLevel
    ModeConfig {..} = modeConfig
