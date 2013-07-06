{-# LANGUAGE OverloadedStrings #-}

module Data.Plist.Encode.XML
  ( toXML
  , toXMLDocument
  ) where

import Data.Plist.Types
import Data.Text (Text)

import qualified Data.ByteString.Base64 as Base64
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Text.XML as X

mkElem :: X.Name -> X.Element
mkElem name = X.Element
  { X.elementName = name
  , X.elementAttributes = Map.empty
  , X.elementNodes = []
  }

mkElemChildren :: X.Name -> [X.Node] -> X.Element
mkElemChildren name nodes
  = (mkElem name) { X.elementNodes = nodes }

mkElemContent :: X.Name -> Text -> X.Element
mkElemContent name text
  = mkElemChildren name [X.NodeContent text]

toXML :: Value -> X.Element
toXML (String x) = mkElemContent "string" x
toXML (Real x) = mkElemContent "real" . Text.pack $ show x
toXML (Integer x) = mkElemContent "integer" . Text.pack $ show x
toXML (Boolean True) = mkElem "true"
toXML (Boolean False) = mkElem "false"
toXML (Data x) = mkElemContent "data" . Text.decodeUtf8 $ Base64.encode x
toXML (Array xs) = mkElemChildren "array" $ map (X.NodeElement . toXML) xs

toXML (Dictionary xs) = mkElemChildren "dict"
  . map X.NodeElement . flatten2 . map mkPair $ Map.toList xs
  where
    mkPair (k, v) = (keyToXML k, toXML v)
    keyToXML = mkElemContent "key"

toXMLDocument :: Value -> X.Document
toXMLDocument rootValue = X.Document
  { X.documentPrologue = prologue
  , X.documentRoot = root
  , X.documentEpilogue = []
  }

  where
    -- <plist version="1.0">
    root = X.Element
      { X.elementName = "plist"
      , X.elementAttributes = Map.singleton "version" "1.0"
      , X.elementNodes = [X.NodeElement $ toXML rootValue]
      }

    -- <?xml version="1.0" encoding="UTF-8"?>
    -- <!DOCTYPE plist PUBLIC "-//Apple Computer//DTD PLIST 1.0//EN"
    --   "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
    prologue = X.Prologue
      { X.prologueBefore = []
      , X.prologueDoctype = Just . X.Doctype "plist" . Just
        $ X.PublicID "-//Apple Computer//DTD PLIST 1.0//EN"
          "http://www.apple.com/DTDs/PropertyList-1.0.dtd"
      , X.prologueAfter = []
      }

flatten2 :: [(a, a)] -> [a]
flatten2 = foldr (\ (x, y) z -> x : y : z) []
