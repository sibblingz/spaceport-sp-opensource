{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Command.Push
  ( Push(..)
  , parse
  , exec
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Char
import Data.Function
import Data.IORef
import Data.Monoid
import Data.Text (Text)
import Data.Typeable
import Development.Shake.FilePath
import Options.Applicative hiding (str)
import Options.Applicative.Types
import System.IO
import System.IO.Unsafe

import qualified Control.Monad.Parallel as Par
import qualified Data.List
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Development.Shake as Shake
import qualified Network as Net
import qualified Network.BSD as Net
import qualified Network.Socket as Net

import Command.Common
import Control.Concurrent.SpExtras
import Development.Spaceport.BuildConfig
import Development.Spaceport.BuildTarget
import Development.Spaceport.Core.Target
import Development.Spaceport.GameFile
import Development.Spaceport.Util
import ModeConfig
import Server
import Sp

import qualified Network.Spaceport.Commander as Comm
import qualified Network.Spaceport.Console as Con
import qualified Network.Spaceport.Neighborhood as Hood

data RemoteHost = RemoteHost Net.HostAddress Net.PortNumber
  deriving (Eq)

data Push
  = ListForever
  | Push
  { modeConfig :: ModeConfig
  , deviceNames :: [ResidentName]
  }

newtype ResidentName = ResidentName { unResidentName :: Text }

instance Eq ResidentName where
  (==) = (==) `on` (Text.toCaseFold . unResidentName)

residentNameFromText :: Text -> ResidentName
residentNameFromText = ResidentName . filterName
  where
    filterName
      = Text.intercalate (Text.singleton '_')
      . Text.words
      . Text.strip . Text.map
        (\ c -> if isAlphaNum c then c else ' ')

residentName :: Hood.Resident -> ResidentName
residentName = residentNameFromText . Hood.name

parse :: CommandParser Push
parse f = command "push" . info parseArgs $ mconcat
  [ fullDesc
  , progDesc
    "Launches an application via WiFi on a mobile device running Spaceport."
  ]
  where
    parseArgs = f $ push <|> listForever

    push = Push
      <$> parseModeConfig
      <*> arguments parseResidentName (mconcat
        [ help "Device identifier."
        , metavar "NAME"
        , completer deviceNameCompleter
        ])

    listForever = ListForever <$ flag' () (mconcat
      [ long "list"
      , help "List devices."
      ])

    parseResidentName = Just . residentNameFromText . Text.pack

resolveDeviceNameToHost :: ResidentName -> IO (Maybe RemoteHost)
resolveDeviceNameToHost name = do
  let match r = residentName r == name
  mResident <- forkTimeout (1 * seconds)
    $ Hood.findResident match
  case mResident of
    Nothing -> return Nothing
    Just r -> do
      hostEntry <- Net.getHostByName $ Hood.hostName r
      return . Just $ RemoteHost
        (Net.hostAddress hostEntry)
        (Hood.commanderPort r)

-- Taken from Network.Socket (network-2.4.0.1).
addressHostString :: Net.SockAddr -> String
addressHostString addr = case addr of
#if !defined(mingw32_HOST_OS) && !defined(_WIN32)
  Net.SockAddrUnix str -> str
#endif
  Net.SockAddrInet _port ha -> unsafePerformIO (Net.inet_ntoa ha)
  Net.SockAddrInet6 _ _ _ _ -> "[" ++ hostString ++ "]"
    where
      hostString = unsafePerformIO $ do
        str <- fst <$> Net.getNameInfo [Net.NI_NUMERICHOST] True False addr
        maybe (fail "Impossible internal error.") return str

launchOnRemoteHost
  :: Net.PortNumber
  -> RemoteHost
  -> IO ()
launchOnRemoteHost serverPort (RemoteHost deviceHost devicePort) = do
  putStrLn $ "Connecting to device at "
    ++ show (Net.SockAddrInet devicePort deviceHost)
  commander <- Comm.connectCommander $ Net.SockAddrInet devicePort deviceHost
  console <- Con.connectConsole $ Net.SockAddrInet (succ devicePort) deviceHost

  -- Get the server's IP on the appropriate
  -- interface/subnet so the device can connect to us
  -- properly.
  serverAddr <- Net.getSocketName $ Comm.commanderSocket commander
  let
    url = concat
      [ "http://", addressHostString serverAddr
      , ":", show serverPort, "/manifest.xml"
      ]

  putStrLn $ "Launching " ++ url ++ " on device..."
  Comm.sendCommand commander (Comm.Launch url) . Just $ \ reply
    -> case reply of
      Comm.Success -> Comm.closeCommander commander
      Comm.Ready -> Comm.closeCommander commander
  _ <- forkIO $ Comm.recvCommandLoop commander falseM

  Con.recvConsoleLoop console Text.putStrLn
  Comm.closeCommander commander

listAndPrintDevicesForever :: IO ()
listAndPrintDevicesForever = Hood.listResidents
  (showUpdate "+") (showUpdate "-")
  where
    showUpdate prefix
      = Text.putStrLn . mappend prefix . unResidentName . residentName

listDevices :: IO [Hood.Resident]
listDevices = do
  devices <- newIORef []
  -- See [note listDevices timeout].
  lock <- newEmptyMVar
  shortDelay <- async $ threadDelay listShortTimeout >> takeMVar lock
  longDelay <- async $ threadDelay listLongTimeout
  let onResidentAdded r = do
        modifyIORef devices (r :)
        void $ tryPutMVar lock ()
  list <- async $ Hood.listResidents onResidentAdded falseM
  void $ waitAnyCancel [shortDelay, longDelay, list]
  -- See [note nub devices].
  Data.List.nub <$> readIORef devices

  where
    listShortTimeout = seconds `div` 4
    listLongTimeout = 2 * seconds

-- [Note listDevices timeout]:
-- We have two timeouts for listing devices: one timeout
-- which will cancel listing if there is any activity, and
-- one timeout which will cancel listing if there is no
-- activity.  The second timeout is used in case listing the
-- devices throws an exception, but takes a while to throw
-- that exception.  (This happens on Windows, for example,
-- if connecting to the Bonjour Windows service fails.)

-- [Note nub devices]:
-- We remove duplicates from the device list because the
-- same device may appear on two different network
-- interfaces.  This is common if you are connected via WiFi
-- and also via ethernet.

printDeviceList :: IO ()
printDeviceList = do
  devices <- listDevices
  if null devices
    then hPutStrLn stderr "No devices running Spaceport found on local network"
    else mapM_ printResident devices
  where printResident = Text.putStrLn . unResidentName . residentName

listDeviceNames :: IO [Text]
listDeviceNames
  = map (unResidentName . residentName)
  <$> listDevices

deviceNameCompleter :: Completer
deviceNameCompleter = listIOCompleter
  $ map Text.unpack <$> listDeviceNames

data DeviceNotFound = DeviceNotFound ResidentName
  deriving (Typeable)

instance Show DeviceNotFound where
  show (DeviceNotFound name)
    = "Could not find device '"
    ++ Text.unpack (unResidentName name)
    ++ "' on local network"

instance Exception DeviceNotFound where

exec :: CommandExecutor Push
exec ListForever = liftIO listAndPrintDevicesForever
exec Push {..} = case deviceNames of
  [] -> liftIO printDeviceList
  _ -> do
    hosts <- liftIO . forM deviceNames $ \ name -> do
      mHost <- resolveDeviceNameToHost name
      case mHost of
        Nothing -> throwIO $ DeviceNotFound name
        Just host -> return host

    buildDir <- getOption buildDirectory
    let gameFileListPath = buildDir
          </> getPath (GameFilesFile manifestMode)
    runShake $ Shake.want [gameFileListPath]

    liftIO $ do
      gameFiles <- readGameFileListFile gameFileListPath
      (_threadID, port) <- startStaticServer buildDir
        $ map (makeGameFileRelative buildDir) gameFiles

      void $ Par.mapM (launchOnRemoteHost port) hosts

  where
    manifestMode = ManifestMode jsMode modeApplicationSource
    jsMode = JSMode modeOptimizationLevel
    ModeConfig {..} = modeConfig
