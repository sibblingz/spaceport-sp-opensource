module Server
  ( startStaticServer
  , startStaticServer'
  , prepareStaticServer
  , findSocketWithPort
  , serveGameFiles
  ) where

import Control.Applicative
import Control.Concurrent
import Data.List
import Data.Maybe
import System.FilePath
import System.IO.Error

import qualified Data.Text as Text
import qualified Filesystem.Path.CurrentOS as FSPath
import qualified Network as Net
import qualified Network.BSD as Net
import qualified Network.Socket as Net
import qualified Network.Socket
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Application.Static as Static
import qualified WaiAppStatic.Types as Static

import Development.Spaceport.GameFile
import Development.Spaceport.Util

startStaticServer
  :: FilePath    -- ^ Root directory containing the files.
  -> [GameFile]  -- ^ Files to serve.
  -> IO (ThreadId, Net.PortNumber)
startStaticServer = startStaticServer' portsToTry
  where
    -- We try to create a socket on a consistent port.
    -- Spaceport's native code sucks and clears its cache if
    -- the entry URL changes.
    portsToTry
      = [ 8272          -- Arbitrary, consistent port.
        , 10234
        , Net.aNY_PORT  -- Fall back to any port.
        ]

startStaticServer'
  :: [Net.PortNumber]
  -> FilePath    -- ^ Root directory containing the files.
  -> [GameFile]  -- ^ Files to serve.
  -> IO (ThreadId, Net.PortNumber)
startStaticServer' ports root files = do
  (serverSocket, runServer)
    <- prepareStaticServer ports root files
  threadID <- forkIO runServer
  port <- Network.Socket.socketPort serverSocket
  return (threadID, port)

prepareStaticServer
  :: [Net.PortNumber]
  -> FilePath    -- ^ Root directory containing the files.
  -> [GameFile]  -- ^ Files to serve.
  -> IO (Net.Socket, IO ())
prepareStaticServer ports root gameFiles = do
  mSocket <- findSocketWithPort ports
  socket <- case mSocket of
    Nothing -> fail $ "Failed to bind to port " ++ portList ports
    Just socket -> return socket
  return (socket, serveGameFiles socket root gameFiles)

  where
    portList = orList . map show . filter (/= Net.aNY_PORT)

-- | Find the first 'Just' under a monad.
maybeSumM :: (Monad m) => [m (Maybe a)] -> m (Maybe a)
maybeSumM [] = return Nothing
maybeSumM (m:ms) = m >>= maybe (maybeSumM ms) (return . Just)

findSocketWithPort
  :: [Net.PortNumber]
  -> IO (Maybe Net.Socket)
findSocketWithPort = maybeSumM . map maybeSocketWithPort
  where
    maybeSocketWithPort port
      = catchIf isAlreadyInUseError
        (Just <$> socketWithPort port)
        (return Nothing)

    -- TODO Clean up socket on error.
    socketWithPort port = do
      s <- Net.socket Net.AF_INET Net.Stream Net.defaultProtocol
      Net.bindSocket s $ Net.SockAddrInet
        port Net.iNADDR_ANY {- Should this be any or only localhost? -}
      Net.listen s maxBound
      return s

serveGameFiles
  :: Network.Socket.Socket
  -> FilePath
  -> [GameFile]
  -> IO ()
serveGameFiles socket root gameFiles
  = Warp.runSettingsSocket warpSettings socket
    $ Static.staticApp staticSettings
  where
    warpSettings = Warp.defaultSettings
      { Warp.settingsFdCacheDuration = 0 }

    defaultStaticSettings = Static.defaultWebAppSettings
      $ FSPath.decodeString root

    -- We wrap the default file handler because the
    -- convenient FS code in the wai-app-static package
    -- is not exposed.
    staticSettings = defaultStaticSettings
      { Static.ssLookupFile = maybe
        (return Static.LRNotFound)
        serveGameFile
        . findGameFile
      }

    serveGameFile
      = Static.ssLookupFile defaultStaticSettings
      . pathToPieces . localPath

    findGameFile pieces
      = find ((== pieces) . pathToPieces . gamePath) gameFiles

pathToPieces :: FilePath -> Static.Pieces
pathToPieces
  = mapMaybe (Static.toPiece . Text.pack)
  . splitDirectories
