module Development.Spaceport.Core.Target
  ( JSMode(..)
  , ManifestMode(..)
  , GameZipMode(..)

  , JSFile(..)
  , ManifestFile(..)
  , AssetFile(..)
  , AssetListFile(..)
  , EmbedDepFile(..)
  , GameZipFile(..)
  , LoadingScreenFile(..)
  , GameFilesFile(..)
  , LibrarySGFFile(..)
  , LibrarySWFFile(..)
  , EmbedFile(..)

  , listGameFilesAction
  ) where

import Development.Shake
import Development.Shake.FilePath
import Development.Spaceport.BuildTarget
import Development.Spaceport.GameFile

data JSMode = JSMode OptimizationLevel
data ManifestMode = ManifestMode JSMode ApplicationSource
data GameZipMode = GameZipMode ManifestMode

instance Permutations JSMode where
  permute = [JSMode opt | opt <- permute]

instance Permutations ManifestMode where
  permute
    = [ ManifestMode jsMode appSource
      | jsMode <- permute
      , appSource <- permute
      ]

instance Permutations GameZipMode where
  permute
    = [ GameZipMode manifestMode
      | manifestMode <- permute
      ]

instance HasPath JSMode where
  getPath (JSMode opt) = getPath opt

instance HasPath ManifestMode where
  getPath (ManifestMode js appSource)
    = js `joinMode` appSource

instance HasPath GameZipMode where
  getPath (GameZipMode manifestMode)
    = getPath manifestMode

data JSFile = JSFile JSMode
data ManifestFile = ManifestFile ManifestMode
data EmbedDepFile = EmbedDepFile JSMode
data GameZipFile = GameZipFile GameZipMode
data AssetFile = AssetFile FilePath
data AssetListFile = AssetListFile
data LoadingScreenFile = LoadingScreenFile
data GameFilesFile = GameFilesFile ManifestMode

data LibrarySGFFile = LibrarySGFFile FilePath
data LibrarySWFFile = LibrarySWFFile FilePath
data EmbedFile = EmbedFile JSMode FilePath

instance HasPath JSFile where
  getPath (JSFile mode) = "js" </> getPath mode </> "game.js"

instance HasPath ManifestFile where
  getPath (ManifestFile mode) = "manifest" </> getPath mode </> "manifest.xml"

instance HasPath EmbedDepFile where
  getPath (EmbedDepFile mode) = "js" </> getPath mode </> "embed.dep"

instance HasPath GameZipFile where
  getPath (GameZipFile mode)
    = "game-zip" </> getPath mode </> "game.zip"

instance HasPath AssetFile where
  getPath (AssetFile path) = "assets" </> path

instance HasPath AssetListFile where
  getPath AssetListFile = "assets.lst"

instance HasPath LoadingScreenFile where
  getPath LoadingScreenFile = "loading_screen" </> "loading_screen.sgf"

instance HasPath GameFilesFile where
  getPath (GameFilesFile mode)
    = "game-files" </> getPath mode

instance HasPath LibrarySGFFile where
  getPath (LibrarySGFFile path) = "library_sgf" </> path

instance HasPath LibrarySWFFile where
  getPath (LibrarySWFFile path) = "library_swf" </> path

instance HasPath EmbedFile where
  getPath (EmbedFile mode path)
    = "embedded_assets" </> getPath mode </> path

listGameFilesAction
  :: ManifestMode -> FilePath -> Action [GameFile]
listGameFilesAction mode root = do
  need [listPath]
  map (mapLocalPath (root </>))
    `fmap` liftIO (readGameFileListFile listPath)
  where listPath = root </> getPath (GameFilesFile mode)
