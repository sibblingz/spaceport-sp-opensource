{-# LANGUAGE OverloadedStrings #-}

module Data.Plist.Decode.XML
  ( fromXML
  , fromXMLDocument
  ) where

import Control.Monad
import Data.Plist.Types
import Data.Text (Text)

import qualified Data.ByteString.Base64 as Base64
import qualified Data.Map as Map
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Read as TextRead
import qualified Text.XML as X
import qualified Text.XML.DOMExtras as X

fromXMLDocument :: X.Document -> Either String Value
fromXMLDocument doc = do
  let root = X.documentRoot doc

  unless (X.hasName "plist" root)
    $ Left "Not a valid plist file; missing <plist> element."
  case X.lookupAttr "version" root of
    Just "1.0" -> return ()
    Just version -> Left $ "Unknown plist version '" ++ show version ++ "'."
    Nothing -> Left "Missing plist version."

  case X.elementChildren root of
    [rootValue] -> fromXML rootValue
    _ -> Left "Expected only one child of <plist>."

fromXML :: X.Element -> Either String Value
fromXML element = case X.elementName element of
  "string" -> Right $ String content
  "real" -> fmap Real $ readWith TextRead.rational
  "integer" -> fmap Integer $ readWith (TextRead.signed TextRead.decimal)
  "true" -> Right $ Boolean True
  "false" -> Right $ Boolean False
  "data" -> fmap Data . Base64.decode $ Text.encodeUtf8 content
  "array" -> fmap Array $ mapM fromXML children
  "dict" -> fmap (Dictionary . Map.fromList) $ readPairs children
  name -> Left $ "Expected value, not <" ++ show name ++ "> element."

  where
    children = X.elementChildren element
    content = X.content element

    readWith f = fmap fst $ f content

    readPairs :: [X.Element] -> Either String [(Text, Value)]
    readPairs = go []
      where
        go xs [] = Right xs
        go _xs [_] = Left "Expected value in <dict>."
        go xs (k : v : rest) = do
          unless (X.hasName "key" k)
            $ Left "Expected <key> element in <dict>."

          let k' = X.content k
          v' <- fromXML v
          xs' <- go xs rest
          return $ (k', v') : xs'
