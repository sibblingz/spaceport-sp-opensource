module Network.Spaceport.Neighborhood
  ( Resident(..)
  , listResidents
  , findResident
  ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Data.Bits
import Data.Text (Text)
import Network.MDNS
import Network.MDNS.API
import Network.MDNS.Foreign

import qualified Data.Text as Text
import qualified Network

import Development.Spaceport.Util

data Resident = Resident
  { hostName :: Network.HostName
  , commanderPort :: Network.PortNumber
  , consolePort :: Network.PortNumber
  , name :: Text
  } deriving (Eq)

instance Show Resident where
  show r = concat
    [ Text.unpack $ name r, " @ "
    , hostName r, ":"
    , show $ commanderPort r, "/", show $ consolePort r
    ]

listResidents
  :: (Resident -> IO ())  -- ^ Added callback.
  -> (Resident -> IO ())  -- ^ Removed callback.
  -> IO ()
listResidents onResidentAdded onResidentRemoved
  = browseServices defaultBrowseOptions
    { browseRegistrationType = "_spaceportapp._tcp" }
    onService

  where
    onService serviceReply = do
      reply <- resolveService
        $ resolveOptionsFromBrowseReply serviceReply

      let added = (browseReplyFlags serviceReply .&. kDNSServiceFlagsAdd) /= 0
      let port = fromIntegral $ resolveReplyPortNumber reply
      let onResident = if added then onResidentAdded else onResidentRemoved
      onResident Resident
        { hostName = resolveReplyHostTarget reply
        , commanderPort = port
        , consolePort = succ port
        , name = Text.pack $ browseReplyServiceName serviceReply
        }

findResident
  :: (Resident -> Bool)
  -> IO Resident
findResident f = do
  found <- newEmptyMVar
  let callback r = when (f r) $ putMVar found r
  withAsync (listResidents callback falseM)
    $ \ _ -> takeMVar found
