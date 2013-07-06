module Network.Spaceport.Console
  ( Console
  , createConsole
  , connectConsole
  , closeConsole

  , recvConsoleLoop

  , Protocol.Message
  ) where

import Control.Monad
import Control.Monad.IO.Class

import qualified Network.Socket as Net

import Network.Spaceport.Common
import Network.Spaceport.Protocol.Console as Protocol

newtype Console = Console
  { consoleSocket :: Net.Socket }

createConsole :: Net.Socket -> IO Console
createConsole = return . Console

connectConsole :: Net.SockAddr -> IO Console
connectConsole = createConsole <=< connect

closeConsole :: Console -> IO ()
closeConsole = closeSocket . consoleSocket

recvConsoleLoop
  :: (MonadIO m)
  => Console
  -> Callback m Protocol.Message
  -> m ()
recvConsoleLoop console callback
  = recvLoop (consoleSocket console) $ \ chunks -> do
    let (mMessage, rest) = getMessage chunks

    case mMessage of
      -- FIXME Eating errors is bad.
      Left _err -> return ()
      Right message -> callback message

    return rest
