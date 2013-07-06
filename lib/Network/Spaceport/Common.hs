{-# LANGUAGE CPP #-}

module Network.Spaceport.Common
  ( Callback

  , connect
  , closeSocket

  , recvLoop
  ) where

import Control.Monad
import Control.Monad.IO.Class

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSLazy
import qualified Data.ByteString.Lazy.Internal as BSLazy
import qualified Network.Socket as Net
import qualified Network.Socket.ByteString as NetBS

type Callback m a = a -> m ()

familyFromAddr :: Net.SockAddr -> Net.Family
familyFromAddr addr = case addr of
  Net.SockAddrInet _ _ -> Net.AF_INET
  Net.SockAddrInet6 _ _ _ _ -> Net.AF_INET6
#if !defined(mingw32_HOST_OS) && !defined(_WIN32)
  Net.SockAddrUnix _ -> Net.AF_UNIX
#endif

connect :: Net.SockAddr -> IO Net.Socket
connect addr = do
  socket <- Net.socket
    (familyFromAddr addr) Net.Stream
    Net.defaultProtocol
  Net.connect socket addr
  return socket

closeSocket :: Net.Socket -> IO ()
#if MIN_VERSION_network(2, 4, 0)
closeSocket = Net.close
#else
closeSocket = Net.sClose
#endif

recvLoop
  :: (MonadIO m)
  => Net.Socket
  -> (BSLazy.ByteString -> m BSLazy.ByteString)
  -> m ()
recvLoop socket handleData = readData BSLazy.empty
  where
    readData chunks
      | BSLazy.null chunks = do
        chunk <- liftIO $ NetBS.recv socket BSLazy.defaultChunkSize
        unless (BS.null chunk)
          . readData $ BSLazy.fromChunks [chunk]
      | otherwise = readData =<< handleData chunks
