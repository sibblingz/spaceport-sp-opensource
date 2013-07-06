module Network.Spaceport.Commander
  ( Commander
  , commanderSocket
  , createCommander
  , connectCommander
  , closeCommander

  , sendCommand
  , recvCommandLoop

  , Protocol.CommandType(..)
  , Protocol.ResponseType(..)
  ) where

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class
import Data.Map (Map)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSLazy
import qualified Data.Map as Map
import qualified Network.Socket as Net
import qualified Network.Socket.ByteString as Net

import Network.Spaceport.Common
import Network.Spaceport.Protocol.Commander as Protocol

data Commander = Commander
  { commanderCallbacks :: MVar
    (Map SequenceID (Callback IO Protocol.ResponseType))
  , commanderSequenceID :: MVar SequenceID
  , commanderSocket :: Net.Socket
  }

nextSequenceID :: Commander -> IO SequenceID
nextSequenceID commander
  = modifyMVar (commanderSequenceID commander) $ \ seqID
    -> return (succ seqID, seqID)

createCommander :: Net.Socket -> IO Commander
createCommander socket = do
  callbacks <- newMVar Map.empty
  sequenceID <- newMVar $ Protocol.SequenceID 1
  return Commander
    { commanderCallbacks = callbacks
    , commanderSequenceID = sequenceID
    , commanderSocket = socket
    }

connectCommander :: Net.SockAddr -> IO Commander
connectCommander = createCommander <=< connect

closeCommander :: Commander -> IO ()
closeCommander = closeSocket . commanderSocket

sendCommand
  :: Commander
  -> Protocol.CommandType
  -> Maybe (Callback IO Protocol.ResponseType)
  -> IO ()
sendCommand commander command mCallback = do
  seqID <- nextSequenceID commander
  case mCallback of
    Just callback -> modifyMVar_ (commanderCallbacks commander)
      $ return . Map.insert seqID callback
    Nothing -> return ()

  let cmd = Command command (Just seqID)
  let socket = commanderSocket commander

  -- We use strict ByteStrings here due to lack of library
  -- support on Windows.
  Net.sendAll socket . toStrict $ putCommand cmd

toStrict :: BSLazy.ByteString -> BS.ByteString
toStrict = BS.concat . BSLazy.toChunks

recvCommandLoop
  :: (MonadIO m)
  => Commander
  -> Callback m Protocol.ResponseType
  -> m ()
recvCommandLoop commander callback
  = recvLoop (commanderSocket commander) $ \ chunks -> do

    -- Read response.
    let (mResponse, rest) = getResponse chunks

    case mResponse of
      -- FIXME Eating errors is bad.
      Nothing -> return ()
      Just (Protocol.Response response mSeqID) -> do
        -- Call response callback and remove from map.
        liftIO $ case mSeqID of
          Nothing -> return ()
          Just seqID -> modifyMVar_ (commanderCallbacks commander)
            $ \ callbacks -> do
              case Map.lookup seqID callbacks of
                Just seqCallback -> seqCallback response
                Nothing -> return ()
              return $ Map.delete seqID callbacks

        -- Call master callback.
        callback response

    return rest
