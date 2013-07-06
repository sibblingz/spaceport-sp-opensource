{-# OPTIONS_GHC -pgmPcpphs -optP--cpp #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Network.MDNS.Load.Windows
  ( withAPI
  ) where

import Control.Concurrent.Async
import Control.Exception as Ex
import Data.Typeable
import Data.Word
import Foreign.C.String
import Foreign.Ptr

import Development.Spaceport.Util
import Network.MDNS.Foreign
import Network.MDNS.Load.Common

data Module
type HModule = Ptr Module

foreign import stdcall "Windows.h LoadLibraryA"
  foreign_LoadLibraryA :: CString -> IO HModule

foreign import stdcall "Windows.h GetProcAddress"
  foreign_GetProcAddress :: HModule -> CString -> IO (FunPtr a)

foreign import stdcall "Windows.h GetLastError"
  foreign_GetLastError :: IO Word32

foreign import stdcall "Windows.h SetLastError"
  foreign_SetLastError :: Word32 -> IO ()

data WindowsAPIError = WindowsAPIError Word32
  deriving (Show, Typeable)

instance Exception WindowsAPIError where

-- TODO Nice error message, a la
-- http://stackoverflow.com/questions/455434/how-should-i-use-formatmessage-properly-in-c
throwIfWinError :: IO a -> IO a
throwIfWinError m = wait =<< asyncBound go
  where
    go = do
      foreign_SetLastError 0
      ret <- m
      err <- foreign_GetLastError
      if err /= 0
        then throwIO $ WindowsAPIError err
        else return ret

loadLibrary :: String -> IO HModule
loadLibrary libName = withCString libName $ \ libNameP
  -> throwIfWinError $ foreign_LoadLibraryA libNameP

getProcAddress :: HModule -> String -> IO (FunPtr a)
getProcAddress hModule procName = withCString procName $ \ procNameP
  -> throwIfWinError $ foreign_GetProcAddress hModule procNameP

dllName :: String
dllName = "dnssd.dll"

catchWinError :: IO a -> (WindowsAPIError -> IO a) -> IO a
catchWinError = Ex.catch

loadAPI :: IO MDNSAPI
loadAPI = do
  hModule <- loadLibrary dllName
    `catchWinError` throwLoadError ("Failed to load " ++ dllName)

#define LOAD(name) \
  fn##name <- fmap foreign_dynamic_##name (getProcAddress hModule #name) \
    `catchWinError` throwLoadError \
      ("Could not find symbol " ++ show #name ++ " in " ++ dllName)
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

withAPI :: (MDNSAPI -> IO a) -> IO a
withAPI = bracket loadAPI falseM  -- TODO Unload.
