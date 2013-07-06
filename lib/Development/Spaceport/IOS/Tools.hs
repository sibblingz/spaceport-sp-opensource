{-# LANGUAGE OverloadedStrings #-}

module Development.Spaceport.IOS.Tools
  ( createUnsignedAppBundle
  , ipaFromAppBundle
  ) where

import Control.Applicative
import Development.Shake
import Development.Shake.FilePath

import qualified Codec.Archive.Zip as Zip

import Development.ShakeExtras
import Development.Spaceport.GameFile
import Development.Spaceport.Util

import qualified Codec.Archive.ZipExtras as Zip
import qualified Data.IOS.InfoPlist as InfoPlist
import qualified Data.Plist as Plist

createUnsignedAppBundle
  :: FilePath    -- ^ Path to template app bundle.
  -> [GameFile]  -- ^ Game files to bundle.
  -> InfoPlist.Settings
  -> FilePath    -- ^ App bundle output location.
  -> Action ()
createUnsignedAppBundle templateBundle gameFiles iOSSettings output = do
  copyRecursive templateBundle output (const True)
  copyGameFilesAction (output </> "assets") gameFiles

  let plistFile = output </> "Info.plist"
  liftIO
      $ Plist.readBinary plistFile
    >>= unEitherString . InfoPlist.applySettings iOSSettings
    >>= Plist.writeBinary plistFile

ipaFromAppBundle
  :: FilePath  -- ^ App bundle (.app).
  -> FilePath  -- ^ Output IPA (.ipa).
  -> Action ()
ipaFromAppBundle appBundle output = do
  entries <- map addIPADirPrefix <$> zipEntriesDeep appBundle
  liftIO . Zip.writeArchive output $ Zip.archiveFromEntries entries
  where
    addIPADirPrefix entry = entry
      { Zip.eRelativePath = "Payload/built.app" </> Zip.eRelativePath entry }
