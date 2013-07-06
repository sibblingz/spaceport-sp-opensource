module Development.Spaceport.Android.Target
  ( AndroidMode(..)

  , AndroidFile(..)
  , AndroidDebugKeyStoreFile(..)
  ) where

import Development.Shake.FilePath

import Development.Spaceport.BuildTarget
import Development.Spaceport.Core.Target

data AndroidMode = AndroidMode ManifestMode SpaceportBinary SigningMode

instance Permutations AndroidMode where
  permute
    = [ AndroidMode manifestMode spBinary signMode
      | manifestMode <- permute
      , spBinary <- permute
      , signMode <- permute
      ]

instance HasPath AndroidMode where
  getPath (AndroidMode manifestMode spBinary signMode)
    = manifestMode `joinMode` spBinary `joinMode` signMode

data AndroidFile = AndroidFile AndroidMode FilePath

instance HasPath AndroidFile where
  getPath (AndroidFile mode path)
    = "android" </> getPath mode </> path

data AndroidDebugKeyStoreFile = AndroidDebugKeyStoreFile

instance HasPath AndroidDebugKeyStoreFile where
  getPath AndroidDebugKeyStoreFile
    = "android" </> "debug.keystore"
