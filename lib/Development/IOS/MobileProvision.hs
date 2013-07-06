{-# LANGUAGE OverloadedStrings #-}

module Development.IOS.MobileProvision
  ( DeploymentType(..)
  , MobileProvisionInfo(..)

  , listMobileProvisionFiles
  , readMobileProvisionFile
  , listMobileProvisionSigningIdentities
  , mobileProvisionContainsIdentity
  , verifyPKCS12
  , p12NeedsPassword
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Set (Set)
import System.Directory
import System.FilePath
import System.Process.Runner

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSLazy
import qualified Data.ByteString.Lazy.Char8 as BSLazy hiding (pack)
import qualified Data.Set as Set

import Development.IOS.Device
import Development.IOS.Identity
import Development.Spaceport.RunTool
import Development.Spaceport.Tools.SpSigner
import Development.Spaceport.Util
import System.Process.Handle

data DeploymentType = Development | Distribution
  deriving (Eq, Ord, Bounded, Enum, Show)

data MobileProvisionInfo = MobileProvisionInfo
  { provisionedDevices :: Set UUID
  , appID :: String
  , appIDName :: String
  , deploymentType :: DeploymentType
  , valid :: Bool
  } deriving (Show)

instance FromJSON MobileProvisionInfo where
  parseJSON (Object o) = MobileProvisionInfo
    <$> (parseUUIDs <$> o .: "provisionedDevices")
    <*> o .: "application-identifier"
    <*> o .: "name"
    <*> (parseDeploymentType <$> o .: "release")
    <*> o .: "valid"
    where
      parseDeploymentType True = Distribution
      parseDeploymentType False = Development
      parseUUIDs = Set.fromList . map UUID
  parseJSON _ = empty

listMobileProvisionFiles :: IO [FilePath]
listMobileProvisionFiles = do
  -- Xcode installs them here on *my* machine.
  dirPath <- (</> "Library/MobileDevice/Provisioning Profiles")
    <$> getHomeDirectory
  exists <- doesDirectoryExist dirPath
  if exists
    then map (dirPath </>) . filter isMobileProvisionPath
      <$> getDirectoryContents dirPath
    else return []
  where
    isMobileProvisionPath p
      = takeExtension p == ".mobileprovision"

lazyFromStrict
  :: BS.ByteString -> BSLazy.ByteString
lazyFromStrict = BSLazy.pack . BS.unpack

readMobileProvisionFile
  :: (MonadIO m)
  => Runner m SpSigner
  -> FilePath
  -> m MobileProvisionInfo
readMobileProvisionFile signer mobileProvision = do
  jsons <- liftM (BSLazy.lines . lazyFromStrict . fst)
    . runHidden "read mobile provision file"
    $ signer GetMobileProvisionInfo
      { mobileProvisionFile = mobileProvision
      , outputAsJson = True
      }
  case map decode $ filter (not . BSLazy.null) jsons of
    [Just info] -> return info
    _ -> fail "Parse error"  -- FIXME

listMobileProvisionSigningIdentities
  :: (MonadIO m)
  => Runner m SpSigner
  -> FilePath
  -> m [Identity]
listMobileProvisionSigningIdentities signer mobileProvision
  = liftM (extractKeychainIdentities . fst)
  . runHidden "list mobile provision signing identities"
  $ signer ListSigningIdentities
    { mMobileProvisionFile = Just mobileProvision
    , identitySources = [AppleKeychain]
    }
  where
    extractKeychainIdentities
      = map (IdentityKeychain . KeychainIdentity)
      . filter (not . null)
      . ioLines . bytestringToStringUTF8

errorStringOrUnit
  :: Either BS.ByteString a -> Either String ()
errorStringOrUnit = either
  (Left . bytestringToStringUTF8)
  (const $ Right ())

mobileProvisionContainsIdentity
  :: (MonadIO m)
  => Runner m SpSigner
  -> FilePath
  -> Identity
  -> m (Either String ())
mobileProvisionContainsIdentity signer mobileProvision ident
  = liftM errorStringOrUnit
  $ liftIO . askProcessOutput
  =<< signer VerifyIdentity
    { mobileProvisionFile = mobileProvision
    , identity = ident
    }

verifyPKCS12
  :: (MonadIO m)
  => Runner m SpSigner
  -> FilePath  -- ^ .p12 file.
  -> String    -- ^ Password.
  -> m (Either String ())
verifyPKCS12 signer p12Path password
  = liftM errorStringOrUnit
  $ liftIO . askProcessOutput
  =<< signer ListSigningIdentities
    { mMobileProvisionFile = Nothing
    , identitySources = [PKCS12File p12Path (Just (return password))]
    }

p12NeedsPassword
  :: (MonadIO m)
  => Runner m SpSigner
  -> FilePath
  -> m Bool
p12NeedsPassword signer p12Path
  = liftM isLeft $ verifyPKCS12 signer p12Path ""
