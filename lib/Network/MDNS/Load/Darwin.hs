module Network.MDNS.Load.Darwin
  ( withAPI
  ) where

import Control.Exception
import Foreign.Ptr
import System.Posix.DynamicLinker

import Development.Spaceport.Util
import Network.MDNS.Foreign
import Network.MDNS.Load.Posix

dlglobal :: IO DL
dlglobal = fmap DLHandle $ c_dlopen nullPtr 0

withAPI :: (MDNSAPI -> IO a) -> IO a
withAPI m = bracket
  dlglobal
  falseM  -- Don't unload the global module.
  (\ dl -> loadAPIFromDL dl >>= m)
