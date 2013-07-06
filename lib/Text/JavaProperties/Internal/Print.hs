{-# LANGUAGE OverloadedStrings #-}

module Text.JavaProperties.Internal.Print
  ( fileContents
  , escapeKey
  , escapeValue
  , makeKeyEntry
  , makeKeyValueEntry
  , makeComment
  ) where

import Data.Monoid
import Data.Text (Text)
import Numeric

import qualified Data.Text as Text

import Text.JavaProperties.Internal.Types

fileContents :: File -> Text
fileContents (File x _ _) = x

showHex4 :: (Show a, Integral a) => a -> Text
showHex4 num
  = Text.justifyRight numDigits '0'
  $ Text.take numDigits digits
  where
    numDigits = 4
    digits = Text.pack $ showHex num ""

-- FIXME This function should handle characters which must
-- be encoded using surrogate pairs in UTF-16, but does not!
escapeValueChar :: Char -> Text
escapeValueChar ' ' = "\\ "
escapeValueChar '\\' = "\\\\"
escapeValueChar '\n' = "\\n"
escapeValueChar '\t' = "\\t"
escapeValueChar '\r' = "\\r"
escapeValueChar '!' = "\\!"
escapeValueChar '#' = "\\#"
escapeValueChar c
  | code < 0x20 || code >= 0x7F
  = "\\u" <> showHex4 code
  | otherwise = Text.singleton c
  where code = fromEnum c

escapeKeyChar :: Char -> Text
escapeKeyChar ':' = "\\:"
escapeKeyChar '=' = "\\="
escapeKeyChar c = escapeValueChar c

escapeKey :: Key -> Text
escapeKey text
  | Text.null text
    = error "Text.JavaProperties.Internal.Print.escapeKey: Key cannot be empty"
  | otherwise = Text.concatMap escapeKeyChar text

escapeValue :: Value -> Text
escapeValue = Text.concatMap escapeValueChar

makeKeyEntry :: LineEnding -> Key -> Text
makeKeyEntry endl key = escapeKey key <> lineEndingText endl

makeKeyValueEntry
  :: LineEnding
  -> Key -> Value
  -> (Text, Range)
makeKeyValueEntry endl key value = (entryText, valueRange)
  where
    keyText = escapeKey key
    valueText = escapeValue value
    separator = " = "

    entryText
      = keyText <> separator <> valueText
      <> lineEndingText endl

    valueRange = Range start end
      where
        start = Text.length keyText + Text.length separator
        end = start + Text.length valueText

makeComment :: LineEnding -> Text -> Text
makeComment endl
  = Text.intercalate (lineEndingText endl)
  . map ("# " <>)
  . Text.lines
