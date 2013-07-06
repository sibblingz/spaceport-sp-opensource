{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE Rank2Types #-}

module Network.MDNS.Foreign where

import Data.Int
import Data.Word
import Foreign.C.String
import Foreign.Ptr
import System.Posix.Types

type DNSServiceError = Int32
type DNSServiceFlags = Word32
type InterfaceIndex = Word32
type PortNumber = Word16

type DNSServiceRef = Ptr DNSServiceRefStruct
data DNSServiceRefStruct

kDNSServiceErr_NoError, kDNSServiceErr_Unknown, kDNSServiceErr_NoSuchName,
  kDNSServiceErr_NoMemory, kDNSServiceErr_BadParam,
  kDNSServiceErr_BadReference, kDNSServiceErr_BadState,
  kDNSServiceErr_BadFlags, kDNSServiceErr_Unsupported,
  kDNSServiceErr_NotInitialized, kDNSServiceErr_AlreadyRegistered,
  kDNSServiceErr_NameConflict, kDNSServiceErr_Invalid, kDNSServiceErr_Firewall,
  kDNSServiceErr_Incompatible, kDNSServiceErr_BadInterfaceIndex,
  kDNSServiceErr_Refused, kDNSServiceErr_NoSuchRecord, kDNSServiceErr_NoAuth,
  kDNSServiceErr_NoSuchKey, kDNSServiceErr_NATTraversal,
  kDNSServiceErr_DoubleNAT, kDNSServiceErr_BadTime, kDNSServiceErr_BadSig,
  kDNSServiceErr_BadKey, kDNSServiceErr_Transient,
  kDNSServiceErr_ServiceNotRunning, kDNSServiceErr_NATPortMappingUnsupported,
  kDNSServiceErr_NATPortMappingDisabled, kDNSServiceErr_NoRouter,
  kDNSServiceErr_PollingMode, kDNSServiceErr_Timeout
  :: DNSServiceError

kDNSServiceErr_NoError                   = 0
kDNSServiceErr_Unknown                   = -65537  -- 0xFFFE FFFF
kDNSServiceErr_NoSuchName                = -65538
kDNSServiceErr_NoMemory                  = -65539
kDNSServiceErr_BadParam                  = -65540
kDNSServiceErr_BadReference              = -65541
kDNSServiceErr_BadState                  = -65542
kDNSServiceErr_BadFlags                  = -65543
kDNSServiceErr_Unsupported               = -65544
kDNSServiceErr_NotInitialized            = -65545
kDNSServiceErr_AlreadyRegistered         = -65547
kDNSServiceErr_NameConflict              = -65548
kDNSServiceErr_Invalid                   = -65549
kDNSServiceErr_Firewall                  = -65550
kDNSServiceErr_Incompatible              = -65551  -- Client library incompatible with daemon.
kDNSServiceErr_BadInterfaceIndex         = -65552
kDNSServiceErr_Refused                   = -65553
kDNSServiceErr_NoSuchRecord              = -65554
kDNSServiceErr_NoAuth                    = -65555
kDNSServiceErr_NoSuchKey                 = -65556
kDNSServiceErr_NATTraversal              = -65557
kDNSServiceErr_DoubleNAT                 = -65558
kDNSServiceErr_BadTime                   = -65559  -- Codes up to here existed in Tiger.
kDNSServiceErr_BadSig                    = -65560
kDNSServiceErr_BadKey                    = -65561
kDNSServiceErr_Transient                 = -65562
kDNSServiceErr_ServiceNotRunning         = -65563  -- Background daemon not running.
kDNSServiceErr_NATPortMappingUnsupported = -65564  -- NAT doesn't support NAT-PMP or UPnP.
kDNSServiceErr_NATPortMappingDisabled    = -65565  -- NAT supports NAT-PMP or UPnP but it's disabled by the administrator.
kDNSServiceErr_NoRouter                  = -65566  -- No router currently configured (probably no network connectivity).
kDNSServiceErr_PollingMode               = -65567
kDNSServiceErr_Timeout                   = -65568

{-
typedef void (DNSSD_API *DNSServiceBrowseReply)
(
    DNSServiceRef sdRef,
    DNSServiceFlags flags,
    uint32_t interfaceIndex,
    DNSServiceErrorType errorCode,
    const char                          *serviceName,
    const char                          *regtype,
    const char                          *replyDomain,
    void                                *context
)
-}
type DNSServiceBrowseReply a
   = DNSServiceRef
  -> DNSServiceFlags
  -> InterfaceIndex
  -> DNSServiceError
  -> CString
  -> CString
  -> CString
  -> Ptr a
  -> IO ()

kDNSServiceFlagsMoreComing, kDNSServiceFlagsAdd, kDNSServiceFlagsDefault,
  kDNSServiceFlagsNoAutoRename, kDNSServiceFlagsShared, kDNSServiceFlagsUnique,
  kDNSServiceFlagsBrowseDomains, kDNSServiceFlagsRegistrationDomains,
  kDNSServiceFlagsLongLivedQuery, kDNSServiceFlagsAllowRemoteQuery,
  kDNSServiceFlagsForceMulticast, kDNSServiceFlagsForce,
  kDNSServiceFlagsReturnIntermediates, kDNSServiceFlagsNonBrowsable,
  kDNSServiceFlagsShareConnection, kDNSServiceFlagsSuppressUnusable,
  kDNSServiceFlagsTimeout, kDNSServiceFlagsIncludeP2P,
  kDNSServiceFlagsWakeOnResolve, kDNSServiceFlagsBackgroundTrafficClass,
  kDNSServiceFlagsIncludeAWDL
  :: DNSServiceFlags

kDNSServiceFlagsMoreComing             = 0x1
kDNSServiceFlagsAdd                    = 0x2
kDNSServiceFlagsDefault                = 0x4
kDNSServiceFlagsNoAutoRename           = 0x8
kDNSServiceFlagsShared                 = 0x10
kDNSServiceFlagsUnique                 = 0x20
kDNSServiceFlagsBrowseDomains          = 0x40
kDNSServiceFlagsRegistrationDomains    = 0x80
kDNSServiceFlagsLongLivedQuery         = 0x100
kDNSServiceFlagsAllowRemoteQuery       = 0x200
kDNSServiceFlagsForceMulticast         = 0x400
kDNSServiceFlagsForce                  = 0x800
kDNSServiceFlagsReturnIntermediates    = 0x1000
kDNSServiceFlagsNonBrowsable           = 0x2000
kDNSServiceFlagsShareConnection        = 0x4000
kDNSServiceFlagsSuppressUnusable       = 0x8000
kDNSServiceFlagsTimeout                = 0x10000
kDNSServiceFlagsIncludeP2P             = 0x20000
kDNSServiceFlagsWakeOnResolve          = 0x40000
kDNSServiceFlagsBackgroundTrafficClass = 0x80000
kDNSServiceFlagsIncludeAWDL            = 0x100000

{-|
DNSServiceErrorType DNSSD_API DNSServiceBrowse
(
    DNSServiceRef                       *sdRef,
    DNSServiceFlags flags,
    uint32_t interfaceIndex,
    const char                          *regtype,
    const char                          *domain,    /* may be NULL */
    DNSServiceBrowseReply callBack,
    void                                *context    /* may be NULL */
)
-}
type DNSServiceBrowse a
  = Ptr DNSServiceRef
  -> DNSServiceFlags
  -> InterfaceIndex
  -> CString
  -> CString
  -> FunPtr (DNSServiceBrowseReply a)
  -> Ptr a
  -> IO DNSServiceError

{-|
int DNSSD_API DNSServiceRefSockFD(DNSServiceRef sdRef);
-}
type DNSServiceRefSockFD
  = DNSServiceRef -> IO Fd

{-|
DNSServiceErrorType DNSSD_API DNSServiceProcessResult(DNSServiceRef sdRef);
-}
type DNSServiceProcessResult
  = DNSServiceRef -> IO DNSServiceError

{-|
DNSServiceErrorType DNSSD_API DNSServiceResolve
(
    DNSServiceRef                       *sdRef,
    DNSServiceFlags flags,
    uint32_t interfaceIndex,
    const char                          *name,
    const char                          *regtype,
    const char                          *domain,
    DNSServiceResolveReply callBack,
    void                                *context  /* may be NULL */
);
-}
type DNSServiceResolve a
  = Ptr DNSServiceRef
  -> DNSServiceFlags
  -> InterfaceIndex
  -> CString
  -> CString
  -> CString
  -> FunPtr (DNSServiceResolveReply a)
  -> Ptr a
  -> IO DNSServiceError

{-|
void DNSSD_API DNSServiceRefDeallocate(DNSServiceRef sdRef);
-}
type DNSServiceRefDeallocate
  = DNSServiceRef -> IO ()

data MDNSAPI = MDNSAPI
  { apiDNSServiceBrowse :: DNSServiceBrowse ()
  , apiDNSServiceRefSockFD :: DNSServiceRefSockFD
  , apiDNSServiceProcessResult :: DNSServiceProcessResult
  , apiDNSServiceResolve :: DNSServiceResolve ()
  , apiDNSServiceRefDeallocate :: DNSServiceRefDeallocate
  }

{-
typedef void (DNSSD_API *DNSServiceResolveReply)
(
    DNSServiceRef sdRef,
    DNSServiceFlags flags,
    uint32_t interfaceIndex,
    DNSServiceErrorType errorCode,
    const char                          *fullname,
    const char                          *hosttarget,
    uint16_t port,                                   /* In network byte order */
    uint16_t txtLen,
    const unsigned char                 *txtRecord,
    void                                *context
);
-}
type DNSServiceResolveReply a
   = DNSServiceRef
  -> DNSServiceFlags
  -> InterfaceIndex
  -> DNSServiceError
  -> CString
  -> CString
  -> PortNumber
  -> Word16
  -> CString
  -> Ptr a
  -> IO ()
