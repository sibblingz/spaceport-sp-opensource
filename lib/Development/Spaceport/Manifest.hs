{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Development.Spaceport.Manifest
  ( ManifestFile(..)
  , ManifestFileEntry(..)
  , manifestFileNames
  , ManifestPlugin(..)

  , CDN(..)
  , CDNConfig(..)
  , emptyCDN
  , manifestCDNInfo

  , manifestToXML
  , manifestFromXML

  , manifestFromFiles

  , readManifestFile
  , writeManifestFile
  ) where

import Control.Exception
import Control.Monad
import Data.Monoid
import Data.Text (Text)

import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSLazy
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Text.XML as X
import qualified Text.XML.DOMExtras as X

import Development.Spaceport.GameFile
import Development.Spaceport.Util

type Version = Text

data ManifestFile = ManifestFile
  { manifestEntries :: [ManifestFileEntry]
  , manifestPlugins :: [ManifestPlugin]
  , manifestBootFile :: FilePath
  , manifestPlatformVersion :: Version
  , manifestApplicationVersion :: Version
  , manifestDescription :: Text
  }
  deriving (Eq, Show)

data ManifestFileEntry = ManifestFileEntry
  { manifestEntryID :: Text
  , manifestEntryVersion :: Text
  , manifestEntryFileSize :: Int
  }
  deriving (Eq, Show)

manifestFileNames :: ManifestFile -> [FilePath]
manifestFileNames
  = map (Text.unpack . manifestEntryID)
  . manifestEntries

data ManifestPlugin = ManifestPlugin
  { manifestPluginID :: Text
  , manifestPluginVersion :: Version
  }
  deriving (Eq, Show)

type URL = Text

data CDN = CDN
  { cdnKey :: Text
  , cdnManifestURL :: URL
  , cdnURL :: Maybe URL
  , cdnUpdateTime :: Text
  , cdnPackageName :: Text
  }
  deriving (Eq, Show)

data CDNConfig = CDNConfig
  { cdnConfigAuthorizationKey :: Text
  , cdnConfigPackageName :: Text
  }
  deriving (Eq, Show)

emptyCDN :: CDN
emptyCDN = CDN "" "" Nothing "" ""

manifestCDNInfo :: CDNConfig -> ManifestFile -> CDN
manifestCDNInfo cdnConfig f = CDN
  { cdnKey = hashParts keyParts
  , cdnManifestURL = manifestURL
  , cdnURL = Just url
  , cdnUpdateTime = time
  , cdnPackageName = packageName
  }

  where
    time = "time_goes_here"  -- TODO

    packageName = cdnConfigPackageName cdnConfig

    manifestURL = Text.concat
      [ "http://apps.spaceport.io/"
      , packageName
      , "/"
      , sanitizeHash . hashParts $ headerParts f
      , "/manifest.xml"
      ]

    url = Text.concat
      [ "http://cdn.spaceport.io/"
      , packageName
      , "/"
      , sanitizeHash . hashParts $ contentsParts f
      , "/"
      ]

    keyParts
      = [ cdnConfigAuthorizationKey cdnConfig
        , time
        , joinParts $ contentsParts f
        ]

type Parts = [Text]

hashParts :: Parts -> Text
hashParts
  = Text.decodeUtf8
  . Base64.encode . MD5.hash
  . Text.encodeUtf8
  . joinParts

joinParts :: Parts -> Text
joinParts = foldr (\ a b -> a <> "+" <> b) ""

sanitizeHash :: Text -> Text
sanitizeHash
  = Text.replace "/" "_"
  . Text.replace "=" ""

headerParts :: ManifestFile -> Parts
headerParts f
  = [ Text.pack $ manifestBootFile f
    , manifestPlatformVersion f
    ] ++ pluginsParts

  where
    pluginsParts = do
      plugin <- manifestPlugins f
      [manifestPluginID plugin, manifestPluginVersion plugin]

contentsParts :: ManifestFile -> Parts
contentsParts f
  = headerParts f
  ++ map manifestEntryVersion (manifestEntries f)

writeManifestFile
  :: FilePath -> ManifestFile -> CDN -> IO ()
writeManifestFile output manifestFile cdnInfo
  = BSLazy.writeFile output $ X.renderLBS settings xml
  where
    settings = X.def { X.rsPretty = True }
    xml = manifestToXML manifestFile cdnInfo

readManifestFile :: FilePath -> IO (ManifestFile, CDN)
readManifestFile path = do
  contents <- BSLazy.readFile path
  case X.parseLBS X.def contents of
    Left err -> throwIO err
    Right xml -> case manifestFromXML xml of
      Left err -> fail err
      Right ret -> return ret

mkManifestFileEntry :: Text -> FilePath -> IO ManifestFileEntry
mkManifestFileEntry entryID path = do
  contents <- BS.readFile path
  return ManifestFileEntry
    { manifestEntryID = entryID
    , manifestEntryVersion = Text.decodeUtf8 . Base64.encode
      $ MD5.hash contents
    , manifestEntryFileSize = BS.length contents
    }

manifestFromFiles
  :: [GameFile]
  -> IO ManifestFile
manifestFromFiles files = do
  entries <- mapM mkEntry files
  return ManifestFile
      { manifestEntries = entries
      , manifestPlugins = []
      , manifestBootFile = "index.html"
      , manifestPlatformVersion = "4.1"
      , manifestApplicationVersion = "1.0"
      , manifestDescription = ""
      }

  where
    mkEntry GameFile {..} = mkManifestFileEntry
      (Text.pack gamePath) localPath

mkElem :: X.Name -> X.Element
mkElem name = X.Element
  { X.elementName = name
  , X.elementAttributes = Map.empty
  , X.elementNodes = []
  }

manifestToXML :: ManifestFile -> CDN -> X.Document
manifestToXML file cdnInfo
  = X.Document prologue root epilogue
  where
    prologue = X.Prologue [] Nothing []
    epilogue = []

    rootAttrs
      = [ ("boot", Text.pack $ manifestBootFile file)
        , ("platform", manifestPlatformVersion file)
        , ("version", manifestApplicationVersion file)
        , ("hash", hashParts $ contentsParts file)

        , ("package", cdnPackageName cdnInfo)
        , ("time", cdnUpdateTime cdnInfo)
        , ("key", cdnKey cdnInfo)
        , ("url", cdnManifestURL cdnInfo)
        ] ++ case cdnURL cdnInfo of
          Just url -> [("cdn", url)]
          Nothing -> []
        ++ if Text.null $ manifestDescription file
          then []
          else [("description", manifestDescription file)]

    root = (mkElem "manifest")
      { X.elementAttributes = Map.fromList rootAttrs
      , X.elementNodes = map X.NodeElement [plugins, startup]
      }

    startup = (mkElem "startup")
      { X.elementNodes = map (X.NodeElement . mkEntryXML)
        $ manifestEntries file
      }

    mkEntryXML entry = (mkElem "entry")
      { X.elementAttributes = Map.fromList
        [ ("id", manifestEntryID entry)
        , ("filesize", Text.pack . show $ manifestEntryFileSize entry)
        , ("version", manifestEntryVersion entry)
        ]
      }

    plugins = (mkElem "plugins")
      { X.elementNodes = map (X.NodeElement . mkPluginXML)
        $ manifestPlugins file
      }

    mkPluginXML ManifestPlugin {..} = (mkElem "entry")
      { X.elementAttributes = Map.fromList
        [ ("id", manifestPluginID)
        , ("version", manifestPluginVersion)
        ]
      }

entryFromXML :: X.Element -> Either String ManifestFileEntry
entryFromXML element = do
  unless (X.hasName "entry" element)
    $ Left "Not a valid entry; expected element named <entry>."

  entryID <- maybeToEither "Missing 'id' attribute on <entry>."
    $ X.lookupAttr "id" element
  versionText <- maybeToEither "Missing 'version' attribute on <entry>."
    $ X.lookupAttr "version" element
  fileSizeText <- maybeToEither "Missing 'filesize' attribute on <entry>."
    $ X.lookupAttr "filesize" element

  fileSize <- maybeToEither "Invalid file size on <entry>."
    . maybeRead $ Text.unpack fileSizeText

  return ManifestFileEntry
    { manifestEntryID = entryID
    , manifestEntryVersion = versionText
    , manifestEntryFileSize = fileSize
    }

pluginFromXML :: X.Element -> Either String ManifestPlugin
pluginFromXML element = do
  unless (X.hasName "entry" element)
    $ Left "Not a valid plugin; expected element named <entry>."

  entryID <- maybeToEither "Missing 'id' attribute on plugin <entry>."
    $ X.lookupAttr "id" element
  versionText <- maybeToEither "Missing 'version' attribute on plugin <entry>."
    $ X.lookupAttr "version" element

  return ManifestPlugin
    { manifestPluginID = entryID
    , manifestPluginVersion = versionText
    }

manifestFromXML
  :: X.Document
  -> Either String (ManifestFile, CDN)
manifestFromXML doc = do
  let root = X.documentRoot doc

  unless (X.hasName "manifest" root)
    $ Left "Not a valid manifest file; missing <manifest> element."

  bootFile <- maybeToEither "Missing 'boot' attribute on <manifest> element."
    $ X.lookupAttr "boot" root

  pluginsElement <- maybeToEither "Missing <plugins> element."
    $ X.findChild (X.hasName "plugins") root
  plugins <- mapM pluginFromXML $ X.elementChildren pluginsElement

  startupElement <- maybeToEither "Missing <startup> element."
    $ X.findChild (X.hasName "startup") root
  entries <- mapM entryFromXML $ X.elementChildren startupElement

  let
    manifestFile = ManifestFile
      { manifestEntries = entries
      , manifestPlugins = plugins
      , manifestBootFile = Text.unpack bootFile
      , manifestPlatformVersion
        = X.findAttrWithDefault "" "platform" root
      , manifestApplicationVersion
        = X.findAttrWithDefault "" "version" root
      , manifestDescription
        = X.findAttrWithDefault "" "description" root
      }
    cdnInfo = CDN
      { cdnKey = X.findAttrWithDefault "" "key" root
      , cdnManifestURL = X.findAttrWithDefault "" "url" root
      , cdnURL = X.lookupAttr "cdn" root
      , cdnUpdateTime = X.findAttrWithDefault "" "time" root
      , cdnPackageName = X.findAttrWithDefault "" "package" root
      }

  return (manifestFile, cdnInfo)
