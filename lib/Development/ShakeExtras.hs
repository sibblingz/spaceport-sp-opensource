module Development.ShakeExtras
  ( getFilesDeepRelative
  , getFilesDeep
  , findFilesDeepRelative
  , copyRecursive

  , copyFileWithDirectory
  , copyFiles

  , zipEntriesDeep

  , needLibrary

  , fileListingRecursive
  , fileListingPattern
  ) where

import Control.Monad
import Development.Shake
import Development.Shake.FilePath
import System.Directory
import System.Environment.ExecutablePath

import qualified Codec.Archive.Zip as Zip

import Development.Spaceport.Util

import qualified Codec.Archive.ZipExtras as Zip

getFilesDeepRelative :: FilePath -> FilePattern -> Action [FilePath]
getFilesDeepRelative = go ""
  where
    go relPath rootPath pattern
      | shouldIgnoreFile relPath = return []
      | otherwise = do
        let path = rootPath </> relPath
        files <- liftM (map (relPath </>))
          $ getDirectoryFiles path pattern
        subFiles <- getDirectoryDirs path >>= concatMapM
          (\ p -> go (relPath </> p) rootPath pattern)
        return $ files ++ subFiles

getFilesDeep :: FilePath -> FilePattern -> Action [FilePath]
getFilesDeep root
  = liftM (map (root </>))
  . getFilesDeepRelative root

findFilesDeepRelative :: FilePath -> (FilePath -> Bool) -> Action [FilePath]
findFilesDeepRelative root f
  = liftM (filter f) $ getFilesDeepRelative root "*"

copyRecursive
  :: FilePath  -- ^ From.
  -> FilePath  -- ^ To.
  -> (FilePath -> Bool)  -- ^ File filter.
  -> Action ()
copyRecursive from to f
  = copyFiles from to =<< findFilesDeepRelative from f

-- | Copies a file (as with @copyFile'@), creating
-- directories if missing from the output.
copyFileWithDirectory
  :: FilePath  -- ^ From.
  -> FilePath  -- ^ To.
  -> Action ()
copyFileWithDirectory from to = do
  liftIO $ createDirectoryIfMissing True (dropFileName to)
  copyFile' from to

-- | Copies many files into a directory using
-- @copyFileWithDirectory@, preserving relative paths.
copyFiles
  :: FilePath    -- ^ Input path root.
  -> FilePath    -- ^ Output directory.
  -> [FilePath]  -- ^ Relative file paths.
  -> Action ()
copyFiles inRoot outPath = mapM_ $ \ file
  -> copyFileWithDirectory
    (inRoot </> file)
    (outPath </> file)

zipEntriesDeep
  :: FilePath
  -> Action [Zip.Entry]
zipEntriesDeep root = do
  files <- findFilesDeepRelative root (const True)
  liftIO $ Zip.zipEntries root files

-- | Use this action if a rule depends on changes the
-- current executable.
--
-- Generation of some files is performed by this executable.
-- Because programmers make mistakes, we depend on the
-- executable instead of relying on you or me to update a
-- build version number.
needLibrary :: Action ()
needLibrary = do
  exePath <- liftIO getExecutablePath
  need [exePath]

-- | Writes a file depending upon the recursive enumeration
-- of a directory.  Does not depend on individual files.
fileListingRecursive
  :: FilePath  -- ^ Path to enumerate recursively.
  -> FilePath  -- ^ Output path.
  -> Action ()
fileListingRecursive root out = do
  paths <- getFilesDeepRelative root "*"
  writeFileLines out paths

-- | Writes a file depending upon the enumeration of a
-- single directory.  Only lists files Does not depend on
-- individual files.
fileListingPattern
  :: FilePath  -- ^ Directory to enumerate.
  -> FilePattern
  -> FilePath  -- ^ Output path.
  -> Action ()
fileListingPattern root pattern out = do
  paths <- getDirectoryFiles root pattern
  writeFileLines out paths
