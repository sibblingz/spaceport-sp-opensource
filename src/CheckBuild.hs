module CheckBuild
  ( ensureiOSBuildable
  , ensureAndroidBuildable
  ) where

import Control.Monad.IO.Class
import Control.Exception
import Data.Monoid

import Development.Spaceport.IOS.Settings
import System.Process.Runner
import Settings
import Sp

import qualified ConfigureIOS as IOS
import qualified Data.SpaceportSettings as Settings
import qualified Development.IOS.Device as IOS
import qualified Development.Spaceport.Tools.SpSigner as Tool

ensureiOSBuildable
  :: Runner IO Tool.SpSigner
  -> [IOS.UUID]
  -> Sp ()
ensureiOSBuildable signer uuids = do
  (settings, _warnings) <- getSettings
  mBuildable <- liftIO $ IOS.ensureiOSSignable signer settings uuids
  case mBuildable of
    Left err -> liftIO $ throwIO err
    Right Nothing -> return ()
    Right (Just (mobileProvisionFile, identity)) -> do
      let
        identityString = identityToString identity
        settingsChanges = mempty
          { Settings.iOSDevMobileProvisionFile = Just mobileProvisionFile
          , Settings.iOSDevIdentity = Just identityString
          }

      settingsSources <- getSettingsSources
      changedPath <- liftIO $ applyFirstAvailableSettings  settingsChanges
        [p | SettingsFile p <- settingsSources]

      liftIO . putStrLn
        $ "Update configuration file " ++ changedPath

      -- Validate afterwards, in case command-line
      -- settings override the settings we just wrote.
      getSettings >>= \ (settings', _warnings) -> liftIO $ do
        mBuildable' <- IOS.validateiOSSignable signer settings'
        case mBuildable' of
          Left err -> throwIO err
          Right _ -> return ()

-- TODO
ensureAndroidBuildable :: Sp ()
ensureAndroidBuildable = return ()
