{-# OPTIONS_GHC -pgmPcpphs -optP--cpp #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Network.MDNS.Load.Common
  ( MDNSAPILoadError(..)
  , throwLoadError

  , foreign_dynamic_DNSServiceBrowse
  , foreign_dynamic_DNSServiceRefSockFD
  , foreign_dynamic_DNSServiceProcessResult
  , foreign_dynamic_DNSServiceResolve
  , foreign_dynamic_DNSServiceRefDeallocate
  ) where

import Control.Exception
import Data.Maybe
import Data.Typeable
import Foreign.C.Types
import Foreign.Ptr
import System.Posix.Types

import Network.MDNS.Foreign

data MDNSAPILoadError
  = MDNSAPILoadError (Maybe String) (Maybe SomeException)
  deriving (Typeable)

instance Show MDNSAPILoadError where
  show (MDNSAPILoadError mMessage mInner)
    = "Failed to load mDNS API: "
    ++ unlines (catMaybes [mMessage, fmap show mInner])

instance Exception MDNSAPILoadError where

throwLoadError
  :: (Exception e)
  => String -> e -> IO a
throwLoadError message mInner = throwIO
  $ MDNSAPILoadError (Just message) (Just $ toException mInner)

#define DYNAMIC_WRAPPER(name) \
  foreign import DNSSD_CALL "dynamic" \
    foreign_dynamic_##name :: FunPtr name -> name
#define DYNAMIC_WRAPPER_1(name) \
  foreign import DNSSD_CALL "dynamic" \
    foreign_dynamic_##name :: FunPtr (name ()) -> (name ())
DYNAMIC_WRAPPER_1(DNSServiceBrowse)
DYNAMIC_WRAPPER(DNSServiceRefSockFD)
DYNAMIC_WRAPPER(DNSServiceProcessResult)
DYNAMIC_WRAPPER_1(DNSServiceResolve)
DYNAMIC_WRAPPER(DNSServiceRefDeallocate)
#undef DYNAMIC_WRAPPER
#undef DYNAMIC_WRAPPER_1
