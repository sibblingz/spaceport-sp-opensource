{-# LANGUAGE DeriveDataTypeable #-}

module Network.MDNS
  ( ServiceError

  , browseServices
  , resolveService
  ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Map (Map)
import Data.Typeable
import System.IO.Unsafe

import qualified Data.Map as Map

import Network.MDNS.API
import Network.MDNS.Foreign
import Network.MDNS.Load

data ServiceError
  = ErrorUnknown DNSServiceError
  | ErrorNoSuchName
  | ErrorNoMemory
  | ErrorBadParam
  | ErrorBadReference
  | ErrorBadState
  | ErrorBadFlags
  | ErrorUnsupported
  | ErrorNotInitialized
  | ErrorAlreadyRegistered
  | ErrorNameConflict
  | ErrorInvalid
  | ErrorFirewall
  | ErrorIncompatible
  | ErrorBadInterfaceIndex
  | ErrorRefused
  | ErrorNoSuchRecord
  | ErrorNoAuth
  | ErrorNoSuchKey
  | ErrorNATTraversal
  | ErrorDoubleNAT
  | ErrorBadTime
  | ErrorBadSig
  | ErrorBadKey
  | ErrorTransient
  | ErrorServiceNotRunning
  | ErrorNATPortMappingUnsupported
  | ErrorNATPortMappingDisabled
  | ErrorNoRouter
  | ErrorPollingMode
  | ErrorTimeout
  deriving (Eq, Show, Typeable)

instance Exception ServiceError where

codeToServiceError :: DNSServiceError -> Maybe ServiceError
codeToServiceError err
  | err == kDNSServiceErr_NoError = Nothing
  | err == kDNSServiceErr_NoSuchName = Just ErrorNoSuchName
  | err == kDNSServiceErr_NoMemory = Just ErrorNoMemory
  | err == kDNSServiceErr_BadParam = Just ErrorBadParam
  | err == kDNSServiceErr_BadReference = Just ErrorBadReference
  | err == kDNSServiceErr_BadState = Just ErrorBadState
  | err == kDNSServiceErr_BadFlags = Just ErrorBadFlags
  | err == kDNSServiceErr_Unsupported = Just ErrorUnsupported
  | err == kDNSServiceErr_NotInitialized = Just ErrorNotInitialized
  | err == kDNSServiceErr_AlreadyRegistered = Just ErrorAlreadyRegistered
  | err == kDNSServiceErr_NameConflict = Just ErrorNameConflict
  | err == kDNSServiceErr_Invalid = Just ErrorInvalid
  | err == kDNSServiceErr_Firewall = Just ErrorFirewall
  | err == kDNSServiceErr_Incompatible = Just ErrorIncompatible
  | err == kDNSServiceErr_BadInterfaceIndex = Just ErrorBadInterfaceIndex
  | err == kDNSServiceErr_Refused = Just ErrorRefused
  | err == kDNSServiceErr_NoSuchRecord = Just ErrorNoSuchRecord
  | err == kDNSServiceErr_NoAuth = Just ErrorNoAuth
  | err == kDNSServiceErr_NoSuchKey = Just ErrorNoSuchKey
  | err == kDNSServiceErr_NATTraversal = Just ErrorNATTraversal
  | err == kDNSServiceErr_DoubleNAT = Just ErrorDoubleNAT
  | err == kDNSServiceErr_BadTime = Just ErrorBadTime
  | err == kDNSServiceErr_BadSig = Just ErrorBadSig
  | err == kDNSServiceErr_BadKey = Just ErrorBadKey
  | err == kDNSServiceErr_Transient = Just ErrorTransient
  | err == kDNSServiceErr_ServiceNotRunning = Just ErrorServiceNotRunning
  | err == kDNSServiceErr_NATPortMappingUnsupported = Just ErrorNATPortMappingUnsupported
  | err == kDNSServiceErr_NATPortMappingDisabled = Just ErrorNATPortMappingDisabled
  | err == kDNSServiceErr_NoRouter = Just ErrorNoRouter
  | err == kDNSServiceErr_PollingMode = Just ErrorPollingMode
  | err == kDNSServiceErr_Timeout = Just ErrorTimeout
  | otherwise = Just $ ErrorUnknown err

noop :: (Monad m) => m ()
noop = return ()

falseM :: (Monad m) => a -> m ()
falseM = const noop

type Lock = MVar ()

-- | If a lock is empty, a service loop is not ongoing.  If
-- full, a service loop is ongoing.  Use 'runServiceLoop' to
-- ensure a loop is running.
serviceLocks :: MVar (Map DNSServiceRef Lock)
serviceLocks = unsafePerformIO $ newMVar Map.empty
{-# NOINLINE serviceLocks #-}

critical :: Lock -> IO a -> IO a
critical lock = bracket
  (takeMVar lock)
  (putMVar lock)
  . const

getServiceLock :: DNSServiceRef -> IO Lock
getServiceLock ref = modifyMVar serviceLocks $ \ locks
  -> case Map.lookup ref locks of
    Nothing -> do
      lock <- newMVar ()
      return (Map.insert ref lock locks, lock)
    Just lock -> return (locks, lock)

serviceCritical :: DNSServiceRef -> IO a -> IO a
serviceCritical ref m = do
  lock <- getServiceLock ref
  critical lock m

-- TODO Remove entry from serviceLocks.
deallocateService :: MDNSAPI -> DNSServiceRef -> IO ()
deallocateService api
  = void . apiDNSServiceProcessResult api

dnsServiceLoop :: MDNSAPI -> DNSServiceRef -> IO a
dnsServiceLoop api ref = forever $ do
  waitReadServiceRef api ref
  dnsServicePoll api ref

dnsServicePoll :: MDNSAPI -> DNSServiceRef -> IO ()
dnsServicePoll api ref = do
  err <- serviceCritical ref
    $ apiDNSServiceProcessResult api ref
  when (err /= 0)
    $ fail $ "DNSServiceProcessResult error occured:\n  " ++ show err

throwIfServiceError :: DNSServiceError -> IO ()
throwIfServiceError = maybe noop throwIO
  . codeToServiceError

throwIfLeftServiceError :: Either DNSServiceError a -> IO a
throwIfLeftServiceError = either
  (\ e -> throwIfServiceError e >> fail shouldNotHappen)
  return
  where
    shouldNotHappen
      = "Network.MDNS.throwIfLeftServiceError:\
        \ Non-error on left should not happen"

-- | Non-terminating.
browseServices
  :: BrowseOptions
  -> (BrowseReplyData -> IO ())
  -> IO ()
browseServices opts callback = withAPI $ \ api -> bracket
  (throwIfLeftServiceError =<< dnsServiceBrowse api opts callback)
  (deallocateService api)
  $ dnsServiceLoop api

-- | Blocking.
resolveService
  :: ResolveOptions
  -> IO ResolveReplyData
resolveService opts = withAPI $ \ api -> do
  resultVar <- newEmptyMVar
  let resolve = dnsServiceResolve api opts (putMVar resultVar)
  bracket
    (throwIfLeftServiceError =<< resolve)
    cleanUp
    $ \ ref -> do
      _threadId <- forkIO $ dnsServiceLoop api ref
      takeMVar resultVar

  where
    -- FIXME Proper clean-up causes a hang.
    cleanUp = falseM
    --cleanUp = deallocateService
