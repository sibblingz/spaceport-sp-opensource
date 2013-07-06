module Text.XML.DOMExtras
  ( hasName

  , findChild
  , filterChildren
  , elementChildren

  , lookupAttr
  , findAttrWithDefault

  , content
  , contents

  , filterNodesRecursive
  , filterDocumentNodes
  ) where

import Data.Maybe
import Data.Text (Text)

import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Text.XML as X

hasName :: X.Name -> X.Element -> Bool
hasName name element = name == X.elementName element

findChild :: (X.Element -> Bool) -> X.Element -> Maybe X.Element
findChild f = listToMaybe . filterChildren f

filterChildren :: (X.Element -> Bool) -> X.Element -> [X.Element]
filterChildren f = filter f . elementChildren

elementChildren :: X.Element -> [X.Element]
elementChildren element
  = [ child | X.NodeElement child <- X.elementNodes element ]

lookupAttr :: X.Name -> X.Element -> Maybe Text
lookupAttr name = Map.lookup name . X.elementAttributes

findAttrWithDefault
  :: Text -> X.Name -> X.Element -> Text
findAttrWithDefault def name
  = Map.findWithDefault def name . X.elementAttributes

content :: X.Element -> Text
content = Text.concat . contents

contents :: X.Element -> [Text]
contents element
  = [ text | X.NodeContent text <- X.elementNodes element ]

-- | Filters out nodes (like 'filter') recursively from a
-- tree of nodes.
filterNodesRecursive :: (X.Node -> Bool) -> [X.Node] -> [X.Node]
filterNodesRecursive f = mapMaybe go
  where
    go :: X.Node -> Maybe X.Node
    go node
      | f node = case node of
        X.NodeElement el -> Just $ X.NodeElement el
          { X.elementNodes = filterNodesRecursive f $ X.elementNodes el }
        _ -> Just node
      | otherwise = Nothing

filterDocumentNodes :: (X.Node -> Bool) -> X.Document -> X.Document
filterDocumentNodes f doc = doc
  { X.documentRoot = el
    { X.elementNodes = filterNodesRecursive f $ X.elementNodes el }
  }
  where el = X.documentRoot doc
