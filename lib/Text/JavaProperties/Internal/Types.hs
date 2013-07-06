{-# LANGUAGE OverloadedStrings #-}

module Text.JavaProperties.Internal.Types
  ( File(..)
  , LineEnding(..)
  , Entry(..)
  , Key
  , Value

  , Offset
  , Range(..)
  , Located(..)

  , entryKey

  , emptyFile
  , lineEndingText
  , unlocated
  , shiftRange
  , mapEntryOffset
  ) where

import Data.Function
import Data.Text (Text)

data File = File Text LineEnding [Located Entry]
  deriving (Show, Eq)

data LineEnding = CR | LF | CRLF
  deriving (Show, Eq, Bounded, Enum)

data Entry = Entry Key (Maybe (Located Value))
  deriving (Show, Eq)

entryKey :: Entry -> Key
entryKey (Entry k _) = k

type Key = Text
type Value = Text

type Offset = Int
data Range = Range !Offset !Offset
data Located a = Located a Range

emptyFile :: File
emptyFile = File "" LF []

lineEndingText :: LineEnding -> Text
lineEndingText CR = "\r"
lineEndingText LF = "\n"
lineEndingText CRLF = "\r\n"

unlocated :: Located a -> a
unlocated (Located x _) = x

shiftRange :: Range -> Int -> Range
shiftRange (Range start end) offset
  = Range (start + offset) (end + offset)

mapEntryOffset :: (Offset -> Offset) -> (Located Entry -> Located Entry)
mapEntryOffset f (Located (Entry key mValue) range)
  = Located (Entry key (fmap mapValue mValue)) (mapRange range)
  where
    mapValue (Located value r)
      = Located value (mapRange r)
    mapRange (Range s e) = Range (f s) (f e)

instance (Show a) => Show (Located a) where
  showsPrec prec = showsPrec prec . unlocated

instance (Eq a) => Eq (Located a) where
  (==) = (==) `on` unlocated

instance Functor Located where
  fmap f (Located x range) = Located (f x) range
