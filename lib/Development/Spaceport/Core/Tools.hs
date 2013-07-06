{-# LANGUAGE OverloadedStrings #-}

module Development.Spaceport.Core.Tools
  ( genIndex
  ) where

import Data.Monoid
import Data.Text (Text)
import Development.Shake as Shake
import Development.Shake.FilePath
import Numeric

import qualified Data.Text as Text
import qualified Data.Text.IO as Text

genIndex :: FilePath -> FilePath -> FilePath -> Action ()
genIndex entryPoint runtimePath output
  = liftIO . Text.writeFile output
    $ mkIndexContents entryPoint runtimePath

escapeHTML :: Text -> Text
escapeHTML = Text.concatMap escape
  where
    escape '<' = "&lt;"
    escape '>' = "&gt;"
    escape '&' = "&amp;"
    escape c = Text.singleton c

escapeJS :: Text -> Text
escapeJS x = "'" <> Text.concatMap escape x <> "'"
  where
    -- Taken from Data.Aeson.Encode (aeson-0.6.0.2).
    escape '\"' = "\\\""
    escape '\\' = "\\\\"
    escape '\n' = "\\n"
    escape '\r' = "\\r"
    escape '\t' = "\\t"
    escape c
      | c < '\x20' = "\\u" <> Text.justifyRight 4 '0' h
      | otherwise = Text.singleton c
      where h = Text.pack $ showHex (fromEnum c) ""

-- FIXME Composition is incorrect, because browsers ignore
-- entities in <script> elements.  However, Spaceport uses
-- an XML parser.  I'm pretty sure the solution is to escape
-- special HTML characters as \u0026 (for '&'), etc.
escapeHTMLJS :: Text -> Text
escapeHTMLJS = escapeHTML . escapeJS

mkIndexContents :: FilePath -> FilePath -> Text
mkIndexContents entryPoint runtimePath = Text.unlines
  $ [ "<!DOCTYPE html>"
    , "<html>"
      , "<head>"
        , "<title>My Spaceport Project</title>"
        , "<meta charset=\"utf-8\" />"  -- Needed for iOS.
        ] ++ scripts ++
        [ "<script>"
          , "sp.createContext(function (err) {"
            , "if (err) {"
              , "alert(err);"
              , "throw err;"
            , "}"
            , "require(["
              , escapeHTMLJS (Text.pack entryPoint)
            , "], sp.init);"
          , "}, {"
            -- , "swfPath: " ++ escapeHTMLJS (runtimeFile "Spaceport.swf")
          , "});"
        , "</script>"
      , "</head>"
      , "<body>"
        , "<div id=\"flashContent\" style=\"width: 640px; height: 480px;\"></div>"
      , "</body>"
    , "</html>"
    ]

  where
    scripts = map (mkExternalScript . runtimeFile)
      ["spaceport.js", "unrequire.js"]

    mkExternalScript path = Text.concat
      [ "<script src=\""
      , escapeHTML (Text.pack path)
      , "\"></script>"
      ]

    runtimeFile = (runtimePath </>)
