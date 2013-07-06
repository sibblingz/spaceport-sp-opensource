{-# LANGUAGE OverloadedStrings #-}

module Development.ActionScript3.FlashBuilder
  ( listProjects
  , readProject
  , parseDocument
  ) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer
import Data.Maybe
import System.Directory
import System.FilePath

import qualified Data.ByteString.Lazy as BSLazy
import qualified Data.Text as Text
import qualified Text.XML as X
import qualified Text.XML.DOMExtras as X

import Development.ActionScript3.Error
import Development.ActionScript3.Project (Project(..))
import Development.Spaceport.Util

-- | Lists all Flash Builder project file paths within a
-- directory.
listProjects :: FilePath -> IO [FilePath]
listProjects root = do
  exists <- doesFileExist path
  return [path | exists]
  where path = root </> ".actionScriptProperties"

readProject
  :: FilePath -> IO (Either String (Project, [ProjectWarning]))
readProject path = do
  contents <- BSLazy.readFile path
  return
    $ parseDocument
      =<< mapLeft show (X.parseLBS X.def contents)

parseDocument
  :: X.Document -> Either String (Project, [ProjectWarning])
parseDocument (X.Document _ root _) = runWriterT $ do
  unless (X.hasName "actionScriptProperties" root) . lift . Left $ unwords
    [ invalid "project file"
    , missingElement "actionScriptProperties"
    ]

  entryPoint <- lift $ maybeToEither
    (missingAttribute "mainApplicationPath" "actionScriptProperties")
    $ X.lookupAttr "mainApplicationPath" root

  compilerElement <- lift $ maybeToEither
    (missingElement "compiler")
    $ X.findChild (X.hasName "compiler") root

  sourceDir <- lift $ maybeToEither
    (missingAttribute "sourceFolderPath" "compiler")
    $ X.lookupAttr "sourceFolderPath" compilerElement

  let
    compilerSourcePathElements = X.filterChildren
      (X.hasName "compilerSourcePath") compilerElement
    compilerSourcePathEntryElements = concatMap
      (X.filterChildren (X.hasName "compilerSourcePathEntry"))
      compilerSourcePathElements

  extraPaths <- forM compilerSourcePathEntryElements
    $ lift . maybeToEither
    (missingAttribute "path" "compilerSourcePathEntry")
    . X.lookupAttr "path"

  let
    libPathElements = X.filterChildren (X.hasName "libraryPath") compilerElement
    libPathEntryElements = filter (X.hasName "libraryPathEntry")
      $ concatMap X.elementChildren libPathElements

    libraryPaths = mapMaybe getLibraryPath libPathEntryElements
    libraryFiles = mapMaybe getLibraryFile libPathEntryElements

  return Project
    { projectSourceDirs = map Text.unpack $ sourceDir : extraPaths
    , projectLibraryDirs = map Text.unpack libraryPaths
    , projectLibraryFiles = map Text.unpack libraryFiles
    , projectAS3EntryPoint = Text.unpack entryPoint
    }

  where
    getLibraryPath element
      | X.lookupAttr "kind" element == Just "1"
        && X.lookupAttr "linkType" element == Just "1"
        = X.lookupAttr "path" element
      | otherwise = Nothing
    getLibraryFile element
      | X.lookupAttr "kind" element == Just "3"
        && X.lookupAttr "linkType" element == Just "1"
        = X.lookupAttr "path" element
      | otherwise = Nothing
