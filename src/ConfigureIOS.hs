{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module ConfigureIOS
  ( validateiOSSignable
  , ensureiOSSignable
  , discoverMobileProvisionIdentity
  ) where

import Control.Applicative
import Control.Arrow ((&&&))
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Control.Monad.Trans.Maybe
import Data.List
import Data.Maybe
import Data.Typeable
import System.IO
import System.IO.Prompt
import System.IO.Prompt.Password

import qualified Control.Monad.Parallel as Par

import Development.IOS.Identity
import Development.Spaceport.IOS.Settings
import Development.Spaceport.Tools.SpSigner
import Development.Spaceport.Util
import System.Process.Runner

import qualified Data.SpaceportSettings as Settings
import qualified Development.IOS.Device as Device
import qualified Development.IOS.MobileProvision as Prov

parMapWithM
  :: (Par.MonadParallel m)
  => (a -> m b) -> [a] -> m [(a, b)]
parMapWithM f = Par.mapM (\ x -> liftM ((,) x) $ f x)

parFilterM
  :: (Par.MonadParallel m)
  => (a -> m Bool) -> [a] -> m [a]
parFilterM f = liftM catMaybes . Par.mapM f'
  where f' x = liftM (`justIf` x) $ f x

data IOSSigningError
  = MissingIdentity
  | MissingMobileProvisionFile
  | SigningError String
  | UserAborted
  deriving (Typeable)

instance Show IOSSigningError where
  show MissingIdentity = "Missing signing identity"
  show MissingMobileProvisionFile = "Missing mobile provision file"
  show (SigningError message) = "Signing error: " ++ message
  show UserAborted = "User aborted"

instance Exception IOSSigningError where

validateiOSSignable
  :: Runner IO SpSigner
  -> Settings.Settings
  -> IO (Either IOSSigningError (FilePath, Identity))
validateiOSSignable signer Settings.Settings {..} = runEitherT $ do
  identityString <- maybe (left MissingIdentity)
    return iOSDevIdentity
  mpFile <- maybe (left MissingMobileProvisionFile)
    return iOSDevMobileProvisionFile
  identity <- liftIO $ identityFromString signer identityString

  mError <- liftIO $ Prov.mobileProvisionContainsIdentity
    signer mpFile identity
  either (left . SigningError) return mError

  return (mpFile, identity)

-- | Ensures an application can be signed, asking the user
-- for help as needed.  Edits global or local configuration.
ensureiOSSignable
  :: Runner IO SpSigner
  -> Settings.Settings
  -> [Device.UUID]  -- ^ Devices, if any.
  -> IO (Either IOSSigningError (Maybe (FilePath, Identity)))
ensureiOSSignable signer settings uuids = do
  -- Validate configured settings.
  mMPFileIdentity <- validateiOSSignable signer settings

  case mMPFileIdentity of
    Right _ -> return $ Right Nothing
    Left err -> do
      case err of
        SigningError message -> hPutStrLn stderr
          $ "Warning: Verification failed for configured\
            \ signing information: " ++ message
        _ -> return ()

      -- Ask the user for mobile provision and identity.
      mMPFileIdentity' <- discoverMobileProvisionIdentity signer uuids
      return $ Just <$> maybeToEither UserAborted mMPFileIdentity'

-- | Searches for a mobile provision file and identity.
--
-- Sources for identities:
--
-- * Apple Keychain
-- * User query
--
-- Sources for mobile provision profiles:
--
-- * Installed on local machine via Xcode
-- * Installed on given USB-connected iOS devices
-- * User query
--
-- The resulting mobile provision file and identity returned
-- by this function may not match.
discoverMobileProvisionIdentity
  :: Runner IO SpSigner
  -> [Device.UUID]  -- ^ Devices, if any.
  -> IO (Maybe (FilePath, Identity))
discoverMobileProvisionIdentity signer uuids = runMaybeT $ do
  mpFiles <- liftIO $ mobileProvisionFiles uuids
  mpIdentities <- liftIO $ parMapWithM
    (Prov.listMobileProvisionSigningIdentities signer) mpFiles

  let identities = nubBy identitiesMaybeEqual $ concatMap snd mpIdentities
  identity <- MaybeT $ selectIdentity signer identities

  -- Optimization: If we already have the mobile provision
  -- files associated with this identity, don't validate the
  -- identity against the mobile provision files.
  let
    mpIdentitiesWithIdentity = filter
      (any (identitiesMaybeEqual identity) . snd)
      mpIdentities
  validMPFiles <- case map fst mpIdentitiesWithIdentity of
    [] -> liftIO $ parFilterM
      (\ mpFile -> isRight <$> Prov.mobileProvisionContainsIdentity
        signer mpFile identity) mpFiles
    files -> return files

  mpFile <- MaybeT $ selectMobileProvision signer validMPFiles

  return (mpFile, identity)

mobileProvisionFiles
  :: [Device.UUID]  -- ^ Devices, if any.
  -> IO [FilePath]
mobileProvisionFiles _uuids
  = Prov.listMobileProvisionFiles 

askIdentity :: Runner IO SpSigner -> IO (Maybe Identity)
askIdentity signer = runMaybeT $ do
  p12Path <- MaybeT . promptForever $ promptExistingFile ".p12 identity file?"
  password <- ifM (liftIO $ Prov.p12NeedsPassword signer p12Path)
    (Just . return <$> getPasswordOf p12Path)
    (return Nothing)
  return $ IdentityPKCS12File p12Path password
  where
    getPasswordOf :: FilePath -> MaybeT IO String
    getPasswordOf p12Path = MaybeT . promptPassword "Password? "
      $ \ pass -> fmap (const pass)
        <$> Prov.verifyPKCS12 signer p12Path pass

selectIdentity
  :: Runner IO SpSigner
  -> [Identity]
  -> IO (Maybe Identity)
selectIdentity signer [] = askIdentity signer
selectIdentity signer identities = do
  mmIdentity <- promptForever $ promptMenu
    (coreMenuOptions ++ [specifyOption])
    "Which identity should Spaceport use to sign iOS applications?"
  case mmIdentity of
    Just (Just identity) -> return $ Just identity
    Just Nothing -> askIdentity signer
    Nothing -> return Nothing
  where
    coreMenuOptions = map (show &&& Just) identities
    specifyOption = ("Specify .p12 identity file", Nothing)

showMobileProvisionFile
  :: Runner IO SpSigner -> FilePath -> IO String
showMobileProvisionFile signer path
  = Prov.appIDName
  <$> Prov.readMobileProvisionFile signer path

askMobileProvisionFile :: IO (Maybe FilePath)
askMobileProvisionFile = promptForever
  $ promptExistingFile ".mobileprovision file?"

selectMobileProvision
  :: Runner IO SpSigner
  -> [FilePath]
  -> IO (Maybe FilePath)
selectMobileProvision _ [] = askMobileProvisionFile
selectMobileProvision signer paths = do
  coreMenuOptions <- Par.mapM mkOption paths
  mmMPFile <- promptForever $ promptMenu
    (coreMenuOptions ++ [specifyOption])
    "Which mobile provision profile should Spaceport use to sign iOS applications?"
  case mmMPFile of
    Just (Just mpFile) -> return $ Just mpFile
    Just Nothing -> askMobileProvisionFile
    Nothing -> return Nothing
  where
    mkOption path = (\ name -> (name, Just path))
      <$> showMobileProvisionFile signer path
    specifyOption = ("Specify .mobileprovision file", Nothing)
