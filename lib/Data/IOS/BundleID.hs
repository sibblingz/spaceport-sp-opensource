{-# LANGUAGE OverloadedStrings #-}

module Data.IOS.BundleID
  ( BundleID
  , ParseError(..)
  , toText
  , fromText
  ) where

import Data.List (nub)
import Data.Text (Text)
import Prelude hiding (words)

import qualified Data.Text as Text

newtype BundleID = BundleID Text
  deriving (Eq)

instance Show BundleID where
  show = Text.unpack . toText

toText :: BundleID -> Text
toText (BundleID text) = text

data ParseError
  = DisallowedCharacter Char
  | EmptyComponent
  | EmptyInput
  deriving (Eq, Show)

-- https://developer.apple.com/library/ios/#documentation/General/Conceptual/ApplicationDevelopmentOverview/ConfigureYourProject/ConfigureYourProject.html#//apple_ref/doc/uid/TP40011186-CH6-SW14
fromText :: Text -> Either [ParseError] BundleID
fromText text
  | Text.null text = Left [EmptyInput]
  | otherwise = case errors $ Text.splitOn "." text of
    [] -> Right $ BundleID text
    errs -> Left $ nub errs

  where
    errors words = concatMap checkCharacters words
      ++ [EmptyComponent | any Text.null words]

    checkCharacters :: Text -> [ParseError]
    checkCharacters
      = map DisallowedCharacter
      . Text.unpack . Text.filter (not . isWordCharacter)

    isWordCharacter :: Char -> Bool
    isWordCharacter x = x `elem` concat
      [ ['a'..'z']
      , ['A'..'Z']
      , ['0'..'9']
      , ['-']
      ]
