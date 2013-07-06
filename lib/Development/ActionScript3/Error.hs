{-# LANGUAGE OverloadedStrings #-}

module Development.ActionScript3.Error
  ( BuildCommandTime(..)
  , ProjectFileWarning(..)
  , ProjectWarning(..)
  , invalid
  , missingAttribute
  , missingElement
  ) where

import Data.Monoid
import Data.Text (Text)

import qualified Data.Text as Text

import Development.Spaceport.Util

data ProjectFileWarning
  = ProjectFileWarning FilePath ProjectWarning

instance Show ProjectFileWarning where
  show (ProjectFileWarning path warning) = concat
    [ path
    , ": Warning: "
    , show warning
    ]

data BuildCommandTime = PreBuild | PostBuild
  deriving (Eq)

instance Show BuildCommandTime where
  show PreBuild = "pre-build"
  show PostBuild = "post-build"

data ProjectWarning
  = BuildCommandsNotRun BuildCommandTime [Text]
  | IncludingAllClassesFromSWC Text
  deriving (Eq)

instance Show ProjectWarning where
  show warning = case warning of
    BuildCommandsNotRun time commands -> concat
      [ show time
      , " "
      , andList $ map (Text.unpack . ("'" <>) . (<> "'")) commands
      , " will not be run"
      ]
    IncludingAllClassesFromSWC path -> concat
      [ "all classes will be included from library '"
      , Text.unpack path
      , "'"
      ]

invalid :: String -> String
invalid thing = concat ["Invalid ", thing, "."]

missingAttribute :: String -> String -> String
missingAttribute attribute element = concat
  ["Missing attribute '", attribute, "' of element <", element, ">."]

missingElement :: String -> String
missingElement element = concat
  ["Missing element <", element, ">."]
