{-# LANGUAGE RecordWildCards #-}

module Development.Spaceport.GameFile
  ( GameFile(..)
  , mapLocalPath
  , makeGameFileRelative

  , copyGameFileAction
  , copyGameFilesAction

  , zipEntry

  , parseGameFileList
  , writeGameFileList
  , readGameFileListFile
  ) where

import Data.Map (Map)
import Development.Shake
import System.Directory
import System.FilePath

import qualified Codec.Archive.Zip as Zip
import qualified Data.Aeson as Ae
import qualified Data.ByteString.Lazy as BSLazy
import qualified Data.Map as Map

import Development.Spaceport.Util

import qualified Codec.Archive.ZipExtras as Zip

data GameFile = GameFile
  { gamePath :: FilePath
  -- ^ The file's path as the game sees it.
  , localPath :: FilePath
  -- ^ The file's path on the local file system.
  }

mapLocalPath :: (FilePath -> FilePath) -> GameFile -> GameFile
mapLocalPath f gameFile = gameFile
  { localPath = f (localPath gameFile) }

makeGameFileRelative :: FilePath -> GameFile -> GameFile
makeGameFileRelative path = mapLocalPath (makeRelative path)

copyGameFileAction :: FilePath -> GameFile -> Action ()
copyGameFileAction out gameFile = do
  liftIO $ createDirectoryIfMissing True (dropFileName to)
  copyFile' from to
  where
    from = localPath gameFile
    to = out </> gamePath gameFile

copyGameFilesAction :: FilePath -> [GameFile] -> Action ()
copyGameFilesAction = mapM_ . copyGameFileAction

zipEntry :: GameFile -> IO Zip.Entry
zipEntry GameFile{..} = do
  entry <- Zip.readEntry Zip.zipOptions localPath
  return $! Zip.forceZipContents
    entry { Zip.eRelativePath = gamePath }

mapToGameFiles :: Map FilePath FilePath -> [GameFile]
mapToGameFiles = map (uncurry GameFile) . Map.toList

gameFilesToMap :: [GameFile] -> Map FilePath FilePath
gameFilesToMap = Map.fromList . map
  (\ GameFile {..} -> (gamePath, localPath))

parseGameFileList :: BSLazy.ByteString -> Maybe [GameFile]
parseGameFileList = fmap mapToGameFiles . Ae.decode

writeGameFileList :: [GameFile] -> BSLazy.ByteString
writeGameFileList = Ae.encode . gameFilesToMap

readGameFileListFile :: FilePath -> IO [GameFile]
readGameFileListFile path = BSLazy.readFile path
  >>= unMaybe ("Corrupt game file list file: " ++ path)
    . parseGameFileList
