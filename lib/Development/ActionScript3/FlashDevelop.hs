{-# LANGUAGE OverloadedStrings #-}

module Development.ActionScript3.FlashDevelop
  ( listProjects
  , readProject
  , parseDocument
  , parseCommandNames
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer
import Data.Char
import Data.List
import Data.Maybe
import Data.Text (Text)
import System.Directory

import qualified Data.ByteString.Lazy as BSLazy
import qualified Data.Text as Text
import qualified Text.XML as X
import qualified Text.XML.DOMExtras as X

import Development.ActionScript3.Error
import Development.ActionScript3.Project
import Development.Spaceport.Util

-- | Lists all FlashDevelop project file paths within a
-- directory.
listProjects :: FilePath -> IO [FilePath]
listProjects root = do
  files <- getDirectoryContents root
  return $ filter (".as3proj" `isSuffixOf`) files

readProject
  :: FilePath -> IO (Either String (Project, [ProjectWarning]))
readProject path = do
  contents <- BSLazy.readFile path
  let document = mapLeft show (X.parseLBS X.def contents)
  return $ parseDocument =<< document

parseDocument
  :: X.Document -> Either String (Project, [ProjectWarning])
parseDocument (X.Document _ root _) = runWriterT $ do
  unless (X.hasName "project" root)
    . lift $ Left "Not a valid project file; missing <project> element."

  compileTargetsElement <- lift $ maybeToEither (missingElement "compileTargets")
    $ X.findChild (X.hasName "compileTargets") root
  compileElement <- lift $ maybeToEither (missingElement "compile")
    $ X.findChild (X.hasName "compile") compileTargetsElement
  entryPoint <- lift $ maybeToEither (missingAttribute "path" "compile")
    $ X.lookupAttr "path" compileElement

  case X.findChild (X.hasName "preBuildCommand") root of
    Just element -> when (not $ Text.null content)
      . warnBuild PreBuild $ parseCommandNames content
      where content = X.content element
    Nothing -> return ()

  case X.findChild (X.hasName "postBuildCommand") root of
    Just element -> when (not $ Text.null content)
      . warnBuild PostBuild $ parseCommandNames content
      where content = X.content element
    Nothing -> return ()

  classPathsElement <- lift $ maybeToEither (missingElement "classpaths")
    $ X.findChild (X.hasName "classpaths") root
  let
    classElements = X.filterChildren (X.hasName "class") classPathsElement
    classPaths = mapMaybe (X.lookupAttr "path") classElements

  let
    libraryPaths = case X.findChild (X.hasName "libraryPaths") root of
      Nothing -> []
      Just libraryPathsElement -> mapMaybe (X.lookupAttr "path")
        $ X.filterChildren (X.hasName "element") libraryPathsElement
  mapM_ warnIncludeLibrary libraryPaths

  let
    includeLibraryPaths = case X.findChild (X.hasName "includeLibraries") root of
      Nothing -> []
      Just includeLibrariesElement -> mapMaybe (X.lookupAttr "path")
        $ X.filterChildren (X.hasName "element") includeLibrariesElement

  return Project
    { projectSourceDirs = map normalizePathSeparator classPaths
    , projectLibraryDirs = []
    , projectLibraryFiles = map normalizePathSeparator
      $ libraryPaths ++ includeLibraryPaths
    , projectAS3EntryPoint = normalizePathSeparator entryPoint
    }
  where
    warnIncludeLibrary path = tell [IncludingAllClassesFromSWC path]
    warnBuild _ [] = return ()
    warnBuild time commands = tell [BuildCommandsNotRun time commands]

-- | Given a newline-separated list of shell commands, duplicates the
-- parsing behavior of FlashDevelop[1] to retrieve only the command
-- names.
--
-- [1]: https://code.google.com/p/flashdevelop/source/browse/trunk/FD4/External/Tools/FDBuild/Building/BuildEventRunner.cs
parseCommandNames :: Text -> [Text]
parseCommandNames
  = nub . mapMaybe (parseCommandName . Text.strip) . Text.lines
  where
    parseCommandName :: Text -> Maybe Text
    parseCommandName raw = case Text.stripPrefix "DEBUG:" raw
      <|> Text.stripPrefix "RELEASE:" raw of
      Just remainder -> parseCommandName $ Text.strip remainder
      Nothing -> case Text.stripPrefix "\"" raw of
        Just quoted -> Just $ Text.takeWhile (/= '"') quoted
        Nothing -> textToMaybe $ Text.takeWhile (not . isSpace) raw
    textToMaybe :: Text -> Maybe Text
    textToMaybe text = if Text.null text then Nothing else Just text

-- | Converts a backslash-delimited path into a
-- forward-slash-delimited one.
normalizePathSeparator :: Text -> FilePath
normalizePathSeparator = Text.unpack . Text.replace "\\" "/"
