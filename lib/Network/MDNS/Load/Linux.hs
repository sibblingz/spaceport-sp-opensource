module Network.MDNS.Load.Linux
  ( withAPI
  ) where

import Control.Exception
import Foreign.Ptr
import System.Posix.DynamicLinker

import Development.Spaceport.Util
import Network.MDNS.Foreign
import Network.MDNS.Load.Unix

withAPI :: (MDNSAPI -> IO a) -> IO a
withAPI m = bracket
  (dlopen "dns_sd" [RTLD_LOCAL])
  dlclose
  (\ dl -> loadAPIFromDL dl >>= m)
