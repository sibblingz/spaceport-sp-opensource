{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Development.Spaceport.Core.Build
  ( gameRules
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Exception
import Control.Monad
import Data.Aeson ((.:))
import Data.Function
import Data.List
import Data.Maybe
import Data.Text (Text)
import Data.Typeable
import Development.Shake as Shake
import Development.Shake.FilePath

import qualified Codec.Archive.Zip as Zip
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSLazy
import qualified Data.Text as Text

import Development.ShakeExtras
import Development.Spaceport.BuildConfig
import Development.Spaceport.BuildTarget
import Development.Spaceport.Core.Target
import Development.Spaceport.Core.Tools
import Development.Spaceport.GameFile as GameFile
import Development.Spaceport.RunTool
import Development.Spaceport.Support
import Development.Spaceport.Tools.A2J
import Development.Spaceport.Tools.Sgftool
import Development.Spaceport.Tools.Spsx
import Development.Spaceport.Tools.Swc2Swf
import Development.Spaceport.Util

import qualified Codec.Archive.ZipExtras as Zip
import qualified Development.Spaceport.Manifest as Manifest

-- [Note Asset rebuild on new source directory]:
--
-- When you add a source directory, keySourceDirs is
-- updated.  This causes *all* SWF's, MP3's, etc. to
-- rebuild.  This approach is taken because it is the safest
-- solution I could come up with and implement correctly.

keySourceDirs, keyEntryPoint, keySWCs, keySWFs, keyMP3s, keyStaticAssets,
  keyLibraryDirs, keyLibraryFiles, keyLoadingScreenFile :: String

keySourceDirs        = projectKey "source directories"
keyEntryPoint        = projectKey "entry point"
keySWCs              = projectKey "SWCs"
keySWFs              = projectKey "SWFs"
keyMP3s              = projectKey "MP3s"
keyStaticAssets      = projectKey "static assets"
keyLibraryDirs       = projectKey "library directories"
keyLibraryFiles      = projectKey "library files"
keyLoadingScreenFile = projectKey "loading screen file"

projectKey :: String -> String
projectKey = ("Project " ++)

keyDefsDir :: String
keyDefsDir = toolKey "definitions directory"

toolKey :: String -> String
toolKey = ("Tool " ++)

keyAuthorizationKeyHash, keyPackageName, keyDescription :: String
keyAuthorizationKeyHash = cdnKey "authorization key"
keyPackageName          = cdnKey "package name"
keyDescription          = cdnKey "description"

cdnKey :: String -> String
cdnKey = ("CDN " ++)

buildRules :: Support -> Tools Action -> FilePath -> Rules ()
buildRules Support {..} Tools {..} outputPath = do
  mapM_ manifestRules permute
  mapM_ gameFileListRule permute
  mapM_ jsRules permute
  mapM_ gameZipRules permute
  assetRules
  mapM_ embedRules permute
  loadingScreenRules

  where
    obj p = outputPath </> getPath p

    ----------------------------------------------------------------------------

    manifestRules :: ManifestMode -> Rules ()
    manifestRules mode@(ManifestMode jsMode Bundle)
      = obj (ManifestFile mode) *> \ out -> do
        needLibrary
        [description] <- askOracle [keyDescription]

        assetFileNames <- readFileLines $ obj AssetListFile
        swcs <- askOracle [keySWCs]
        deps <- readEmbedDeps jsMode

        let
          files = jsGameFile : map assetGameFile
            ([ "index.html"
             , spaceportOutputPath </> "spaceport.js"
             , spaceportOutputPath </> "unrequire.js"
             ] ++ assetFileNames)
            ++ map libraryGameFile swcs
            ++ map embeddedGameFile deps

        need $ map localPath files

        liftIO $ do
          manifestFile <- Manifest.manifestFromFiles files
          Manifest.writeManifestFile out
            manifestFile
              { Manifest.manifestPlugins = manifestPlugins
              , Manifest.manifestDescription = Text.pack description
              }
            Manifest.emptyCDN

      where
        jsGameFile = GameFile "game.js" (obj $ JSFile jsMode)
        assetGameFile p = GameFile p (obj $ AssetFile p)
        libraryGameFile p = GameFile (getPath sgf) (obj sgf)
          where sgf = LibrarySGFFile $ takeFileName p `replaceExtension` ".sgf"
        embeddedGameFile = mapLocalPath (outputPath </>) . makeEmbedFile jsMode

        -- FIXME Hard-coded list is temporary.  We don't
        -- have full plugin support quite yet.
        manifestPlugins = map (flip Manifest.ManifestPlugin "4.0.0")
          [ "io.spaceport.plugins.contacts"
          , "io.spaceport.plugins.mail"
          , "io.spaceport.plugins.sms"
          ]

    manifestRules mode@(ManifestMode jsMode CDN)
      = obj (ManifestFile mode) *> \ out -> do
        [authKey] <- askOracle [keyAuthorizationKeyHash]
        [packageName] <- askOracle [keyPackageName]

        -- A CDN manifest is the same as a bundle manifest
        -- with added information.
        need [bundleManifestPath]
        (manifest, _bundleCDNInfo) <- liftIO
          $ Manifest.readManifestFile bundleManifestPath

        let cdnConfig = Manifest.CDNConfig
              { Manifest.cdnConfigAuthorizationKey = Text.pack authKey
              , Manifest.cdnConfigPackageName = Text.pack packageName
              }
        let cdnInfo = Manifest.manifestCDNInfo cdnConfig manifest
        liftIO $ Manifest.writeManifestFile out manifest cdnInfo

      where
        bundleManifestPath = obj $ ManifestFile (ManifestMode jsMode Bundle)

    ----------------------------------------------------------------------------

    gameFileListRule :: ManifestMode -> Rules ()
    gameFileListRule mode@(ManifestMode jsMode _)
      = obj (GameFilesFile mode) *> \ out -> do
        assetFileNames <- readFileLines $ obj AssetListFile
        swcs <- askOracle [keySWCs]
        deps <- readEmbedDeps jsMode
        let
          files = jsGameFile : entryPointFile : manifestFile
            : map assetGameFile (assetFileNames
            ++ [ spaceportOutputPath </> "spaceport.js"
               , spaceportOutputPath </> "unrequire.js"
               ])
            ++ map libraryGameFile swcs
            ++ map (makeEmbedFile jsMode) deps

        need $ map (\ gf -> outputPath </> localPath gf) files
        liftIO . BSLazy.writeFile out $ writeGameFileList files
      where
        jsGameFile = GameFile "game.js" (getPath $ JSFile jsMode)
        entryPointFile = GameFile "index.html" (getPath $ AssetFile "index.html")
        manifestFile = GameFile "manifest.xml" (getPath $ ManifestFile mode)
        assetGameFile p = GameFile p (getPath $ AssetFile p)
        libraryGameFile p = GameFile (getPath sgf) (getPath sgf)
          where sgf = LibrarySGFFile $ takeFileName p `replaceExtension` ".sgf"

    ----------------------------------------------------------------------------

    gameZipRules :: GameZipMode -> Rules ()
    gameZipRules mode@(GameZipMode manifestMode)
      = obj (GameZipFile mode) *> \ out -> do
        gameFiles <- listGameFilesAction manifestMode outputPath
        liftIO $ do
          entries <- mapM GameFile.zipEntry gameFiles
          Zip.writeArchive out $ archiveFromEntries entries

        where
          archiveFromEntries
            = Zip.addMissingZipDirectories
            . foldr (Zip.addEntryToArchive . addUpdateDirPrefix)
              Zip.emptyArchive

          addUpdateDirPrefix entry = entry
            { Zip.eRelativePath = "game" </> Zip.eRelativePath entry }

    ----------------------------------------------------------------------------

    assetRules :: Rules ()
    assetRules = do
      let spaceportAsset p = obj . AssetFile $ spaceportOutputPath </> p

      obj AssetListFile *> \ out -> do
        swfs <- askOracle [keySWFs]
        mp3s <- askOracle [keyMP3s]
        staticAssets <- askOracle [keyStaticAssets]

        let
          files = concat
            [ map (`replaceExtension` ".sgf") swfs
            , map (`replaceExtension` ".ogg") mp3s
            , staticAssets
            ]

        need $ map (obj . AssetFile) files
        writeFileLines out files

      spaceportAsset "spaceport.js" *> \ out
        -> copyFile' spaceportLibraryPath out

      spaceportAsset "unrequire.js" *> \ out
        -> copyFile' unrequirePath out

      obj (AssetFile "index.html") *> \ out -> do
        needLibrary
        genIndex "game.js" spaceportOutputPath out

      assetsDir ++ "//*.sgf" *> \ out -> do
        swf <- findAssetSource $ out `replaceExtension` ".swf"
        runConvert swf $ sgftool Sgftool
          { inputSwf = swf
          , outputSgf = out
          }

      obj (LibrarySGFFile "*.sgf") *> \ out -> do
        let swcLibrariesDirectory = obj $ LibrarySGFFile ""  -- HACK
        let swf = obj (LibrarySWFFile $ makeRelative swcLibrariesDirectory out)
              `replaceExtension` ".swf"
        runConvert swf $ sgftool Sgftool
          { inputSwf = swf
          , outputSgf = out
          }

      obj (LibrarySWFFile "*.swf") *> \ out -> do
        -- See [note Asset rebuild on new source directory].
        swc <- swcFromSWF out
        runConvert swc $ swc2Swf Swc2Swf
          { inputSwc = swc
          , outputSwf = out
          }

      assetsDir ++ "//*.ogg" *> \ out -> do
        mp3 <- findAssetSource $ out `replaceExtension` ".mp3"
        runConvert mp3 $ spsx Spsx
          { inputMp3 = mp3
          , outputOgg = out
          , quality = 0.6
          }

      isStaticOutputAsset ?> \ out -> do
        input <- findAssetSource out
        copyFile' input out

      where
        assetsDir = obj (AssetFile "")  -- HACK...

        outToExisting sourceDirs path
          = existingSources sourceDirs path >>= uniqueFile path

        findAssetSource p = do
          -- See [note Asset rebuild on new source directory].
          sourceDirs <- askOracle [keySourceDirs]
          outToExisting sourceDirs $ makeRelative assetsDir p

        isStaticOutputAsset p
          = (p `withinDir` assetsDir) && isStaticAsset p

    ----------------------------------------------------------------------------

    loadingScreenRules :: Rules ()
    loadingScreenRules = obj LoadingScreenFile *> \ out -> do
      [loadingScreenSWF] <- askOracle [keyLoadingScreenFile]
      runConvert loadingScreenSWF $ sgftool Sgftool
        { inputSwf = loadingScreenSWF
        , outputSgf = out
        }

    ----------------------------------------------------------------------------

    jsRules :: JSMode -> Rules ()
    jsRules mode@(JSMode _opt)
      = [obj (JSFile mode), embedDepFile mode] *>> \ [jsOut, depOut] -> do
      [defsDir] <- askOracle [keyDefsDir]
      sourceDirs <- askOracle [keySourceDirs]
      [as3EntryPoint] <- askOracle [keyEntryPoint]

      swcs <- map takeFileName <$> askOracle [keySWCs]
      let swcToSWF file
            = obj . LibrarySWFFile $ file `replaceExtension` ".swf"
      need $ map swcToSWF swcs

      -- See [note A2J library output]
      let swcToSGF file
            = getPath . LibrarySGFFile $ file `replaceExtension` ".swf"

      runConvert "ActionScript 3 code" $ a2j A2J
        { sourcePathDirs = defsDir </> "as3" : sourceDirs
        , entryPoint = as3EntryPoint
        , outputFile = jsOut
        , libraries = for swcs $ \ swc -> A2JLibrary
          { librarySWF = swcToSWF swc
          -- See [note A2J library output].
          , libraryOutput = swcToSGF swc
          }
        , depFile = depOut
        }

      needExistingDeps =<< liftIO (readEmbedDepsIO mode)

    needExistingDeps :: [EmbedDep] -> Action ()
    needExistingDeps deps
      = forM_ deps $ \ EmbedDep{..}
        -> forM_ embedSearchedPaths $ \ path -> do
          exists <- doesFileExist path
          when exists $ need [path]

    embedRules :: JSMode -> Rules ()
    embedRules jsMode = do
      isRawEmbed ?> \ out -> do
        realPath <- getRealPath out
        copyFile' realPath out

      obj (EmbedFile jsMode "*.ogg") *> \ out -> do
        realPath <- getRealPath out
        runConvert realPath $ spsx Spsx
          { inputMp3 = realPath
          , outputOgg = out
          , quality = 0.6
          }

      obj (EmbedFile jsMode "*.sgf") *> \ out -> do
        realPath <- getRealPath out
        runConvert realPath $ sgftool Sgftool
          { inputSwf = realPath
          , outputSgf = out
          }

      where
        isRawEmbed path = path `withinDir` embedDir
          && takeExtension path `notElem` convertedExtensions
        convertedExtensions = [".ogg", ".sgf"]
        embedDir = obj (EmbedFile jsMode "")  -- HACK

        getRealPath :: FilePath -> Action FilePath
        getRealPath out = do
          deps <- readEmbedDeps jsMode
          case embedRealPath <$> find matching deps of
            Just realPath -> return realPath
            Nothing -> liftIO . throwIO $ MissingEmbedDep out
          where
            matching
              = (out ==)
              . (outputPath </>)
              . localPath
              . makeEmbedFile jsMode

    embedDepFile :: JSMode -> FilePath
    embedDepFile = obj . EmbedDepFile

    readEmbedDeps
      :: JSMode
      -> Action [EmbedDep]
    readEmbedDeps jsMode = do
      need [embedDepFile jsMode]
      liftIO $ readEmbedDepsIO jsMode

    readEmbedDepsIO
      :: JSMode
      -> IO [EmbedDep]
    readEmbedDepsIO jsMode = do
      embedDepsJSON <- BSLazy.readFile path
      case Aeson.eitherDecode' embedDepsJSON of
        Left message -> liftIO . throwIO $ MalformedEmbedDepsFile message
        Right deps -> return deps
      where path = obj $ EmbedDepFile jsMode

data MalformedEmbedDepsFile = MalformedEmbedDepsFile String
  deriving (Typeable)

instance Show MalformedEmbedDepsFile where
  show (MalformedEmbedDepsFile message)
    = "invalid [Embed] dependencies file: " ++ message

instance Exception MalformedEmbedDepsFile where

data MissingEmbedDep = MissingEmbedDep FilePath
  deriving (Typeable)

instance Show MissingEmbedDep where
  show (MissingEmbedDep path)
    = "missing [Embed] dependency data for path: " ++ path

instance Exception MissingEmbedDep where

data EmbedDep = EmbedDep
  { embedRealPath :: FilePath
  , embedRuntimePath :: FilePath
  , embedSearchedPaths :: [FilePath]
  }

instance Aeson.FromJSON EmbedDep where
  parseJSON (Aeson.Object o) = EmbedDep
    <$> o .: "real_path"
    <*> o .: "runtime_path"
    <*> o .: "searched_paths"
  parseJSON _ = mzero

makeEmbedFile
  :: JSMode
  -> EmbedDep
  -> GameFile
makeEmbedFile jsMode EmbedDep{..} = GameFile
  { gamePath = realRuntimePath
  , localPath = getPath $ EmbedFile jsMode realRuntimePath
  }
  where
    realRuntimePath = replaceEmbedExtension embedRuntimePath
    replaceEmbedExtension path = case splitExtension path of
      (base, ".mp3") -> base <.> "ogg"
      (base, ".swf") -> base <.> "sgf"
      _ -> path

uniqueFile :: (Monad m) => FilePath -> [FilePath] -> m FilePath
uniqueFile _ [p] = return p
uniqueFile path [] = fail $ "Missing " ++ path
uniqueFile path paths = fail
  $ "Duplicate files found for " ++ path ++ ":\n" ++ unlines paths

swcFromSWF :: FilePath -> Action FilePath
swcFromSWF out
  = uniqueFile (takeFileName out)
  =<< filter (sameBaseName $ out `replaceExtension` ".swc")
  <$> askOracle [keySWCs]

sameBaseName :: FilePath -> FilePath -> Bool
sameBaseName = (==) `on` takeFileName

oracle
  :: ProjectConfig
  -> Manifest.CDNConfig
  -> Support
  -> Rules ()
oracle projectConfig cdnConfig Support {..} = do
  addOracle [keySWFs] $ do
    sourceDirs <- askOracle [keySourceDirs]
    concatMapM (`getFilesDeepRelative` "*.swf") sourceDirs

  addOracle [keyMP3s] $ do
    sourceDirs <- askOracle [keySourceDirs]
    concatMapM (`getFilesDeepRelative` "*.mp3") sourceDirs

  addOracle [keyStaticAssets] $ do
    sourceDirs <- askOracle [keySourceDirs]
    concatMapM (`findFilesDeepRelative` isStaticAsset) sourceDirs

  addOracle [keySWCs] $ do
    libraryDirs <- askOracle [keyLibraryDirs]
    fromDirs <- concatMapM (`getFilesDeep` "*.swc") libraryDirs
    fromFiles <- askOracle [keyLibraryFiles]
    return . nub $ fromDirs ++ fromFiles

  addOracle [keyEntryPoint] $ return
    [projectEntryPoint projectConfig]

  addOracle [keyLibraryDirs] $ return
    $ projectLibraryDirs projectConfig

  addOracle [keyLibraryFiles] $ return
    $ projectLibraryFiles projectConfig

  addOracle [keyLoadingScreenFile] $ return
    [ fromMaybe defaultLoadingScreenPath
      $ projectLoadingScreenFile projectConfig
    ]

  addOracle [keySourceDirs] $ return
    $ projectInputPaths projectConfig

  addOracle [keyDefsDir] $ return [a2jDefsDir]

  addOracle [keyAuthorizationKeyHash] $ return
    [Text.unpack $ Manifest.cdnConfigAuthorizationKey cdnConfig]

  addOracle [keyPackageName] $ return
    [Text.unpack $ Manifest.cdnConfigPackageName cdnConfig]

  addOracle [keyDescription] $ return
    [Text.unpack $ projectDescription projectConfig]

existingSources
  :: [FilePath] -> FilePath -> Action [FilePath]
existingSources roots p
  = filterM doesFileExist
  $ map (</> p) roots

gameRules
  :: ProjectConfig
  -> Manifest.CDNConfig
  -> Support
  -> Tools Action
  -> FilePath
  -> Rules ()
gameRules projectConfig cdnConfig support tools outputPath = do
  oracle projectConfig cdnConfig support
  buildRules support tools outputPath

withinDir :: FilePath -> FilePath -> Bool
withinDir a b = addTrailingPathSeparator b
  `isPrefixOf` addTrailingPathSeparator a

isStaticAsset :: FilePath -> Bool
isStaticAsset f
  | shouldIgnoreFile f = False
  -- Ignore manifest.xml, index.html.
  | takeFileName f `elem` ["manifest.xml", "index.html"] = False
  -- Ignore files outside output directory.
  | f `withinDir` spaceportOutputPath = False
  -- Ignore certain extensions.
  | otherwise = not (fileIgnored || folderIgnored)
    where
      fileIgnored = getExt f `caselessElem` ignoredFileExtensions
      folderIgnored = any (`caselessElem` ignoredFolderExtensions)
        . map getExt . safeInit $ splitDirectories f

      getExt = Text.pack . takeExtension

safeInit :: [a] -> [a]
safeInit [] = []
safeInit xs = init xs

ignoredFileExtensions :: [Text]
ignoredFileExtensions
  = -- Source
    [ ".swf"
    , ".as"
    , ".swc"
    , ".mp3"

    -- Generated
    , ".sgf"
    , ".js"
    , ".ogg"

    -- Commonly-ignored
    , ".ai"
    , ".exe"
    , ".fla"
    , ".htm"
    , ".html"
    , ".psd"
    ]

ignoredFolderExtensions :: [Text]
ignoredFolderExtensions = [".app"]
