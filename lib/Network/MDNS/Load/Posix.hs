{-# OPTIONS_GHC -pgmPcpphs -optP--cpp #-}
{-# LANGUAGE CPP #-}

module Network.MDNS.Load.Posix
  ( loadAPIFromDL
  ) where

import System.IO.Error
import System.Posix.DynamicLinker

import Development.Spaceport.Util
import Network.MDNS.Foreign
import Network.MDNS.Load.Common

catchDLError :: IO a -> (IOError -> IO a) -> IO a
catchDLError = catchWhen isUserError

loadAPIFromDL :: DL -> IO MDNSAPI
loadAPIFromDL dl = do
#define LOAD(name) \
  fn##name <- fmap foreign_dynamic_##name (dlsym dl #name) \
    `catchDLError` throwLoadError \
      ("Could not find symbol " ++ show #name)
  LOAD(DNSServiceBrowse)
  LOAD(DNSServiceRefSockFD)
  LOAD(DNSServiceProcessResult)
  LOAD(DNSServiceResolve)
  LOAD(DNSServiceRefDeallocate)
#undef LOAD

  return MDNSAPI
    { apiDNSServiceBrowse = fnDNSServiceBrowse
    , apiDNSServiceRefSockFD = fnDNSServiceRefSockFD
    , apiDNSServiceProcessResult = fnDNSServiceProcessResult
    , apiDNSServiceResolve = fnDNSServiceResolve
    , apiDNSServiceRefDeallocate = fnDNSServiceRefDeallocate
    }
