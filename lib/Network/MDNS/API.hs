{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Network.MDNS.API
  ( BrowseReplyData(..)
  , BrowseOptions(..)
  , defaultBrowseOptions

  , ResolveReplyData(..)
  , ResolveOptions(..)
  , defaultResolveOptions
  , resolveOptionsFromBrowseReply

  , dnsServiceBrowse
  , dnsServiceResolve

  , waitReadServiceRef
  ) where

import Control.Applicative
import Control.Monad
import Data.Word
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import System.IO.BlockFD

import Network.MDNS.Foreign

foreign import DNSSD_CALL unsafe "ntohs" foreign_ntohs :: Word16 -> Word16

foreign import DNSSD_CALL "wrapper" foreign_wrap_DNSServiceBrowseReply
  :: DNSServiceBrowseReply a -> IO (FunPtr (DNSServiceBrowseReply a))

foreign import DNSSD_CALL "wrapper" foreign_wrap_DNSServiceResolveReply
  :: DNSServiceResolveReply a -> IO (FunPtr (DNSServiceResolveReply a))

data BrowseReplyData = BrowseReplyData
  { browseReplyServiceRef :: DNSServiceRef
  , browseReplyInterface :: InterfaceIndex
  , browseReplyFlags :: DNSServiceFlags
  , browseReplyError :: DNSServiceError
  , browseReplyServiceName :: String
  , browseReplyRegistrationType :: String
  , browseReplyDomain :: String
  }

data BrowseOptions = BrowseOptions
  { browseServiceFlags :: DNSServiceFlags
  , browseInterface :: InterfaceIndex
  , browseRegistrationType :: String
  , browseDomain :: Maybe String
  }

defaultBrowseOptions :: BrowseOptions
defaultBrowseOptions = BrowseOptions
  { browseServiceFlags = 0
  , browseInterface = 0
  , browseRegistrationType = ""
  , browseDomain = Nothing
  }

data ResolveReplyData = ResolveReplyData
  { resolveReplyServiceRef :: DNSServiceRef
  , resolveReplyInterface :: InterfaceIndex
  , resolveReplyFlags :: DNSServiceFlags
  , resolveReplyError :: DNSServiceError
  , resolveReplyFullName :: String
  , resolveReplyHostTarget :: String
  , resolveReplyPortNumber :: PortNumber
  , resolveReplyTextRecord :: String
  }

data ResolveOptions = ResolveOptions
  { resolveServiceFlags :: DNSServiceFlags
  , resolveInterface :: InterfaceIndex
  , resolveServiceName :: String
  , resolveRegistrationType :: String
  , resolveDomain :: String
  }

defaultResolveOptions :: ResolveOptions
defaultResolveOptions = ResolveOptions
  { resolveServiceFlags = 0
  , resolveInterface = 0
  , resolveServiceName = ""
  , resolveRegistrationType = ""
  , resolveDomain = ""
  }

resolveOptionsFromBrowseReply
  :: BrowseReplyData -> ResolveOptions
resolveOptionsFromBrowseReply opts = defaultResolveOptions
  { resolveInterface = browseReplyInterface opts
  , resolveServiceName = browseReplyServiceName opts
  , resolveRegistrationType = browseReplyRegistrationType opts
  , resolveDomain = browseReplyDomain opts
  }

-- TODO Memory cleanup
dnsServiceBrowse
  :: MDNSAPI
  -> BrowseOptions
  -> (BrowseReplyData -> IO ())
  -> IO (Either DNSServiceError DNSServiceRef)
dnsServiceBrowse mDNSAPI opts callback = do
  regtypeP <- newCString $ browseRegistrationType opts
  domainP <- maybeNewCString $ browseDomain opts
  callbackP <- foreign_wrap_DNSServiceBrowseReply cCallback
  refP <- malloc

  err <- apiDNSServiceBrowse mDNSAPI
    refP
    (browseServiceFlags opts)
    (browseInterface opts)
    regtypeP
    domainP
    callbackP
    nullPtr

  if err == kDNSServiceErr_NoError
    then Right <$> peek refP
    else return $ Left err

  where
    cCallback
      ref flags interfaceIndex errorCode
      serviceName regtype domain _context
      = do
      serviceName' <- peekCString serviceName
      regtype' <- peekCString regtype
      domain' <- peekCString domain
      callback BrowseReplyData
        { browseReplyServiceRef = ref
        , browseReplyInterface = interfaceIndex
        , browseReplyFlags = flags
        , browseReplyError = errorCode
        , browseReplyServiceName = serviceName'
        , browseReplyRegistrationType = regtype'
        , browseReplyDomain = domain'
        }

dnsServiceResolve
  :: MDNSAPI
  -> ResolveOptions
  -> (ResolveReplyData -> IO ())
  -> IO (Either DNSServiceError DNSServiceRef)
dnsServiceResolve mDNSAPI opts callback = do
  nameP <- newCString $ resolveServiceName opts
  regtypeP <- newCString $ resolveRegistrationType opts
  domainP <- newCString $ resolveDomain opts
  callbackP <- foreign_wrap_DNSServiceResolveReply cCallback
  refP <- malloc

  err <- apiDNSServiceResolve mDNSAPI
    refP
    (resolveServiceFlags opts)
    (resolveInterface opts)
    nameP
    regtypeP
    domainP
    callbackP
    nullPtr

  if err == kDNSServiceErr_NoError
    then Right <$> peek refP
    else return $ Left err

  where
    cCallback
      ref flags interfaceIndex errorCode fullname
      hosttarget port txtLen txtRecord _context
      = do
      fullname' <- peekCString fullname
      hosttarget' <- peekCString hosttarget
      txtRecord' <- peekCStringLen (txtRecord, fromIntegral txtLen)
      callback ResolveReplyData
        { resolveReplyServiceRef = ref
        , resolveReplyInterface = interfaceIndex
        , resolveReplyFlags = flags
        , resolveReplyError = errorCode
        , resolveReplyFullName = fullname'
        , resolveReplyHostTarget = hosttarget'
        , resolveReplyPortNumber = foreign_ntohs port
        , resolveReplyTextRecord = txtRecord'
        }

waitReadServiceRef :: MDNSAPI -> DNSServiceRef -> IO ()
waitReadServiceRef mDNSAPI
   = blockRead <=< apiDNSServiceRefSockFD mDNSAPI

maybeNewCString :: Maybe String -> IO (Ptr CChar)
maybeNewCString = maybe (return nullPtr) newCString
