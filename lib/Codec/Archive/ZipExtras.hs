module Codec.Archive.ZipExtras
  ( zipOptions

  , zipEntry
  , zipEntries

  , archiveFromEntries

  , readArchive
  , writeArchive

  , forceZipContents

  , addMissingZipDirectories

  , Input(..)
  , InputEntry(..)
  , buildArchive
  ) where

import Control.Monad
import Data.List
import Development.Shake.FilePath

import qualified Data.ByteString.Lazy as BSLazy
import qualified Codec.Archive.Zip as Zip

import Development.Spaceport.Util

zipOptions :: [Zip.ZipOption]
zipOptions = []

-- | ZIP archives always use '/' as the path separator, so
-- we do not use 'System.FilePath'.
zipPathSep :: Char
zipPathSep = '/'

zipEntry
  :: FilePath  -- ^ File root.
  -> FilePath  -- ^ File path.
  -> IO Zip.Entry
zipEntry root file = do
  entry <- Zip.readEntry zipOptions (root </> file)
  return $! forceZipContents
    entry { Zip.eRelativePath = file }

-- Note: Use '/' as a separator.
mapRelativePath
  :: (FilePath -> FilePath)
  -> Zip.Entry -> Zip.Entry
mapRelativePath f entry
  = entry { Zip.eRelativePath = f (Zip.eRelativePath entry) }

zipEntries
  :: FilePath
  -> [FilePath]
  -> IO [Zip.Entry]
zipEntries root = mapM (zipEntry root)

archiveFromEntries :: [Zip.Entry] -> Zip.Archive
archiveFromEntries = addMissingZipDirectories
  . foldr Zip.addEntryToArchive Zip.emptyArchive

readArchive :: FilePath -> IO Zip.Archive
readArchive = liftM Zip.toArchive . BSLazy.readFile

writeArchive :: FilePath -> Zip.Archive -> IO ()
writeArchive path
  = BSLazy.writeFile path . Zip.fromArchive

-- | Forces evaluation (via 'seq') of a ZIP entry's contents
-- to prevent file handle leaks due to lazy I/O.
forceZipContents :: Zip.Entry -> Zip.Entry
forceZipContents entry
  = BSLazy.length (Zip.eCompressedData entry) `seq` entry

-- | Adds missing directory entries to an archive.
--
-- A standard ZIP archive has a directory entry for each
-- directory mentioned.  'Codec.Archive.Zip' allows for
-- construction of an archive without these entries.  This
-- function fixes such archives.
addMissingZipDirectories :: Zip.Archive -> Zip.Archive
addMissingZipDirectories archive
  = foldr addDirectoryToArchive archive
  $ missingZipDirectories archive
  where
    addDirectoryToArchive = Zip.addEntryToArchive . mkDirectoryEntry
    mkDirectoryEntry path = Zip.toEntry path 0 BSLazy.empty

missingZipDirectories :: Zip.Archive -> [String]
missingZipDirectories archive = allDirectories \\ archiveDirectories
  where
    zipTakeDirectory path = case path' of { [] -> path ; _ -> path' }
      where path' = reverse . dropWhile (/= zipPathSep) $ reverse path

    allDirectories =  nub . filter (not . null)
      $ map zipTakeDirectory archivePaths

    archivePaths = map Zip.eRelativePath $ Zip.zEntries archive
    archiveDirectories = filter ([zipPathSep] `isSuffixOf`) archivePaths

data Input = Input FilePath InputEntry

data InputEntry
  = ZipInput FilePath
  | FileInput FilePath

buildArchive :: [Input] -> IO Zip.Archive
buildArchive inputs = do
  entries <- concatMapM mkEntries inputs
  return $ Zip.emptyArchive { Zip.zEntries = entries }
  where
    mkEntries :: Input -> IO [Zip.Entry]
    mkEntries (Input root input) = case input of
      ZipInput path -> do
        archive <- readArchive path
        return . map (mapRelativePath rooted)
          $ Zip.zEntries archive
      FileInput path -> do
        entry <- Zip.readEntry zipOptions path
        return [mapRelativePath (const root) entry]
      where
        rooted = case root of
          "" -> id
          _ -> ((root ++ [zipPathSep]) ++)
