{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Development.Spaceport.BuildTarget
  ( OptimizationLevel(..)
  , ApplicationSource(..)
  , SpaceportBinary(..)
  , SigningMode(..)

  , Permutations(..)

  , HasPath(..)
  , joinMode
  ) where

data OptimizationLevel = Unoptimized | Optimized
  deriving (Eq, Bounded, Enum)
data ApplicationSource = Bundle | CDN
  deriving (Eq, Bounded, Enum)
data SpaceportBinary = SpaceportDebug | SpaceportRelease
  deriving (Eq, Bounded, Enum)
data SigningMode = DeveloperSigning | SubmissionSigning
  deriving (Eq, Bounded, Enum)

class HasPath a where
  getPath :: a -> FilePath

instance HasPath FilePath where
  getPath = id

class Permutations a where
  permute :: [a]

#ifndef HLINT
  -- hlint has trouble parsing DefaultSignatures, so we hide
  -- the next line.
  default permute :: (Bounded a, Enum a) => [a]
  permute = [minBound .. maxBound]
#endif

instance Permutations OptimizationLevel where
instance Permutations ApplicationSource where
instance Permutations SpaceportBinary where
instance Permutations SigningMode where

joinMode :: (HasPath a, HasPath b) => a -> b -> FilePath
a `joinMode` b = getPath a ++ "-" ++ getPath b

instance HasPath OptimizationLevel where
  getPath Unoptimized = "unoptimized"
  getPath Optimized = "optimized"

instance HasPath ApplicationSource where
  getPath Bundle = "bundle"
  getPath CDN = "cdn"

instance HasPath SpaceportBinary where
  getPath SpaceportDebug = "spdebug"
  getPath SpaceportRelease = "sprelease"

instance HasPath SigningMode where
  getPath DeveloperSigning = "devsigned"
  getPath SubmissionSigning = "submission"
