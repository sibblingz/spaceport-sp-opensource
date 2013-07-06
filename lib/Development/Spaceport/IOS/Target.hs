module Development.Spaceport.IOS.Target
  ( IOSMode(..)

  , IOSFile(..)
  ) where

import Development.Shake.FilePath

import Development.Spaceport.BuildTarget
import Development.Spaceport.Core.Target

data IOSMode = IOSMode ManifestMode SpaceportBinary SigningMode

instance Permutations IOSMode where
  permute
    = [ IOSMode manifestMode spBinary signMode
      | manifestMode <- permute
      , spBinary <- permute
      , signMode <- permute
      ]

instance HasPath IOSMode where
  getPath (IOSMode manifestMode spBinary signMode)
    = manifestMode `joinMode` spBinary `joinMode` signMode

data IOSFile = IOSFile IOSMode FilePath

instance HasPath IOSFile where
  getPath (IOSFile mode path)
    = "ios" </> getPath mode </> path

