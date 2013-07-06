module Data.Plist.Types
  ( Value(..)
  ) where

import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Text (Text)

data Value
  = String !Text
  | Real !Double
  | Integer !Integer
  | Boolean !Bool
  | Data !ByteString
  | Array [Value]
  | Dictionary (Map Text Value)
  deriving (Show, Eq)
