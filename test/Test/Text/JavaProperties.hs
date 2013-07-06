{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Text.JavaProperties where

import Data.Text (Text)
import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.Framework.TH
import Test.HUnit
import Test.QuickCheck
import Test.QuickCheck.Instances ()

import qualified Data.Text as Text

import Text.JavaProperties

instance Arbitrary File where
  arbitrary = fmap parse' arbitrary

main = defaultMain tests
tests = [$(testGroupGenerator)]

parse' = fromRight . parseProperties ""

fromRight :: Either e a -> a
fromRight (Right x) = x
fromRight _ = error "fromRight"

newtype K = K Text
  deriving (Eq, Ord, Show, Read)

instance Arbitrary K where
  arbitrary = fmap K
    $ arbitrary `suchThat` (not . Text.null)

  shrink (K text)
    = map K . filter (not . Text.null)
    $ shrink text

prop_round_trip_parse_contents contents
  = fileContents (parse' contents) == contents

prop_round_trip_contents_parse file
  = parse' (fileContents file) == file

prop_setValue file (K k) v
  = lookup k (filePairs file') == Just v
  where file' = setValue k v file

prop_round_trip_setValue file (K k) v
  = file' == parse' (fileContents file')
  where file' = setValue k v file

prop_setValue_overrides file (K k) (NonEmpty vs)
  = lookup k (filePairs file') == Just (last vs)
  where file' = foldl (flip $ setValue k) file vs

prop_round_trip_setValue_deleteKey file (K k) v
  = filePairs file == filePairs file'
  where file' = deleteKey k $ setValue k v file

case_setValue_same_key_replaces
  = "key = value2\n" @=? fileContents file
  where
    file
      = setValue "key" "value2"
      $ setValue "key" "value1"
        emptyFile

case_setValue_same_key_replaces_parsed
  = "#comment\nkey=value2" @=? fileContents file
  where
    file = setValue "key" "value2" origFile
    origFile = parseProperties' "#comment\nkey=value1"

case_setValue_same_key_replaces_parsed_2
  = "#comment\n key : value2\r\nother_key \n" @=? fileContents file
  where
    file
      = setValue "other_key" ""
      $ setValue "key" "value2" origFile
    origFile = parseProperties'
      "#comment\n key : value1   \r\nother_key value\n"

case_setValue_new_key_with_escape_eol
  = lookup "new key" (filePairs file) @?= Just "new value"
  where
    file = setValue "new key" "new value" origFile
    origFile = parseProperties' "k=nasty_value\\"

case_setValue_new_key_with_escape_eol_r = do
  lookup "k" (filePairs file) @?= Just "nasty_value"
  lookup "new key" (filePairs file) @?= Just "new value"
  where
    file = setValue "new key" "new value" origFile
    origFile = parseProperties' "\r\nk=nasty_value\\\r"

case_deleteKey_deletes = do
  lookup "a" (filePairs file) @?= Nothing
  lookup "c" (filePairs file) @?= Just "d"
  where
    file = deleteKey "a" origFile
    origFile = parseProperties' "a = b\nc = d"
