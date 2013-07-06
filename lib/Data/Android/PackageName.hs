{-# LANGUAGE OverloadedStrings #-}

module Data.Android.PackageName
  ( PackageName
  , ParseError(..)
  , toPieces
  , toText
  , fromText
  ) where

import Data.Char
import Data.List (nub)
import Data.Text (Text)
import Prelude hiding (words)

import qualified Data.Text as Text

newtype PackageName = PackageName [Text]
  deriving (Eq)

instance Show PackageName where
  show = Text.unpack . toText

toPieces :: PackageName -> [Text]
toPieces (PackageName pieces) = pieces

packagePieceSeparator :: Text
packagePieceSeparator = "."

toText :: PackageName -> Text
toText = Text.intercalate packagePieceSeparator . toPieces

data ParseError
  = DisallowedComponent Text
  | DisallowedCharacter Char
  | EmptyComponent
  | NeedAtLeastTwoComponents
  | EmptyInput
  deriving (Eq, Show)

fromText :: Text -> Either [ParseError] PackageName
fromText text
  | Text.null text = Left [EmptyInput]
  | otherwise = if null errors
    then Right $ PackageName pieces
    else Left $ nub errors

  where
    pieces = Text.splitOn packagePieceSeparator text

    errors = concat
      [ [NeedAtLeastTwoComponents | length pieces < 2]
      , concatMap checkCharacters pieces
      , concatMap checkKeyword pieces
      ]

    checkCharacters :: Text -> [ParseError]
    checkCharacters word = case Text.uncons word of
      Nothing -> [EmptyComponent]
      Just (initial, rest)
        -> [DisallowedCharacter initial | not (isInitialCharacter initial)]
        ++ [DisallowedCharacter c | c <- Text.unpack rest, not (isWordCharacter c)]

    isInitialCharacter :: Char -> Bool
    isInitialCharacter c = isWordCharacter c && not (isDigit c)

    isWordCharacter :: Char -> Bool
    isWordCharacter c = any ($ c) [isAlpha, isDigit, (== '_')]

    checkKeyword :: Text -> [ParseError]
    checkKeyword word
      = [DisallowedComponent word | word `elem` keywords]

    keywords = Text.words
      "abstract assert boolean break byte case catch char \
      \class const continue default do double else enum \
      \extends final finally float for if goto implements \
      \import instanceof int interface long native new \
      \package private protected public return short \
      \static strictfp super switch synchronized this \
      \throw throws transient try void volatile while"
