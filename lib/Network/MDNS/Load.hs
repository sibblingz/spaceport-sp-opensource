{-# OPTIONS_GHC -pgmPcpphs -optP--cpp #-}
{-# LANGUAGE CPP #-}

module Network.MDNS.Load
  ( Load.withAPI
  ) where

#define CAT(a, b) a##b
#define MODNAME CAT(Network.MDNS.Load., DNSSD_MODULE)
import qualified MODNAME as Load
