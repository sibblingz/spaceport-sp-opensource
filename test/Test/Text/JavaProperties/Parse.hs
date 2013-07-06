{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Text.JavaProperties.Parse where

import Data.Text (Text)
import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.Framework.TH
import Test.HUnit
import Test.QuickCheck.Instances ()

import qualified Data.Text as Text

import Development.Spaceport.Util
import Text.JavaProperties

main = defaultMain tests
tests = $(testGroupGenerator) : concatMap hUnitTestToTests
  [ TestLabel "parse single key-value pair tests" tests_parse_kvp
  , TestLabel "parse multiple key-value pairs tests" tests_parse_kvps
  , TestLabel "parse blank lines" tests_parse_blank_lines
  , TestLabel "parse comments" tests_parse_comments

  , TestLabel "parse multiline keys" tests_parse_multiline_keys
  , TestLabel "parse multiline values" tests_parse_multiline_values

  , TestLabel "parse escaped keys" tests_parse_escaped_keys
  , TestLabel "parse escaped values" tests_parse_escaped_values
  , TestLabel "parse escaped unicode keys" tests_parse_escaped_unicode_keys

  , test_truth_beauty_1
  , test_truth_beauty_2
  , test_truth_beauty_3

  , test_fruits
  ]

sourceName :: String
sourceName = "Test.Text.JavaProperties.Parse"

testParse :: (Either String [(Text, Text)], Text) -> Test
testParse (expected, input)
  = TestLabel (show input)
  $ either (Left . show) (Right . filePairs) actual ~?= expected
  where
    actual = parseProperties sourceName input

tests_parse_kvp = TestList $ map testParse
  [ (Right [("k", "v")], "k=v")
  , (Right [("key", "value")], "key = value")
  , (Right [("key", "value")], "key value")
  , (Right [("key", "value")], "key\tvalue")
  , (Right [("key", "value")], "key\fvalue")
  , (Right [("key", "value")], "key :value")
  , (Right [("key", "=value")], "key:=value")
  , (Right [("key", "value pairs rule!")], "key value pairs rule!")
  , (Right [("key_only", "")], "key_only")
  , (Right [("key_only", "")], "key_only ")
  , (Right [("=key", "value")], "=key=value")
  , (Right [("---", "")], "---")
  ]

tests_parse_kvps = TestList $ map testParse
  [ (Right [("k", "v"), ("v", "k")], "k=v\nv=k")
  ]

tests_parse_blank_lines = TestList $ map testParse
  [ (Right [("k", "v")], "k=v\n")
  , (Right [("k", "v"), ("v", "k")], "k=v\n\n\n\n\nv=k\n\n\n\n")
  , (Right [], "\r\n\r\r\n")
  , (Right [("a", "b")], "\n        \na=b\n \n")
  ]

tests_parse_comments = TestList $ map testParse
  [ (Right [], "#this is a comment")
  , (Right [], "#this is a comment\n")
  , (Right [], " #this is a comment\r")
  , (Right [], "\t #this is a comment\r\n")

  , (Right [], "!this is a comment")
  , (Right [], "!this is a comment\n")
  , (Right [], " !this is a comment\r")
  , (Right [], "\t !this is a comment\r\n")
  ]

tests_parse_multiline_keys = TestList $ map testParse
  [ (Right [("keykey", "")], "key\\key")
  , (Right [("keymorekey", "value")], "key\\\n  morekey value")
  , (Right [("key\\", "")], "key\\")
  ]

tests_parse_multiline_values = TestList $ map testParse
  [ (Right [("key", "valuemore value")], "key value\\\nmore value")
  , (Right [("key", "valuemore value")], "key value\\\n more value")
  , (Right [("key", "value more value")], "key value \\\nmore value")
  , (Right [("super_key", "omg wtf lol")], "super_key = omg \\\n            wtf \\\n            lol")
  , (Right [("key", "\\")], "key = \\")
  ]

-- http://docs.oracle.com/javase/specs/jls/se7/html/jls-3.html#jls-3.10.6
-- with some exceptions, as documented in
-- http://docs.oracle.com/javase/7/docs/api/java/util/Properties.html#load(java.io.Reader)
tests_parse_escaped_keys = TestList $ map testParse
  [ (Right [("key with spaces", "value")], "key\\ with\\ spaces value")
  , (Right [("key=with=equals", "value")], "key\\=with\\=equals=value")
  , (Right [("key:with:colons", "value")], "key\\:with\\:colons:value")
  , (Right [("key\twith\ttabs", "value")], "key\\twith\\ttabs\tvalue")
  , (Right [("key\nwith\nnewlines", ""), ("value", "")], "key\\nwith\\nnewlines\nvalue")
  , (Right [("\n\\", "")], "\\n\\\\\n")

  , (Right [("key\\with\\backslashes", "")], "key\\\\with\\\\backslashes")
  , (Right [("keywithunescapedbackslashes", "")], "key\\with\\unescaped\\backslashes")

  , (Right [("key\"with\"doublequotes", "")], "key\\\"with\\\"doublequotes")
  , (Right [("key\"with\"unescaped\"doublequotes", "")], "key\"with\"unescaped\"doublequotes")

  , (Right [("key\'with\'singlequotes", "")], "key\\\'with\\\'singlequotes")
  , (Right [("key\'with\'unescaped\'singlequotes", "")], "key\'with\'unescaped\'singlequotes")
  ]

tests_parse_escaped_values = TestList $ map testParse
  [ (Right [("key", "value\nwith\nnewline"), ("next", "")], "key value\\nwith\\nnewline\nnext")
  ]

-- http://docs.oracle.com/javase/specs/jls/se7/html/jls-3.html#jls-3.3
-- with some exceptions, as documented in
-- http://docs.oracle.com/javase/7/docs/api/java/util/Properties.html#load(java.io.Reader)
tests_parse_escaped_unicode_keys = TestList $ map testParse
  [ (Right [("key with spaces", "value")], "key\\u0020with\\u0020spaces value")
  , (Right [("key=with=equals", "value")], "key\\u003dwith\\u003Dequals=value")
  , (Right [("key:with:colons", "value")], "key\\u003awith\\u003Acolons:value")
  , (Right [("key\twith\ttabs", "value")], "key\\u0009with\\u0009tabs\tvalue")
  , (Right [("key\nwith\nnewlines", ""), ("value", "")], "key\\u000awith\\u000Anewlines\nvalue")
  , (Right [("key\\with\\backslashes", "")], "key\\u005cwith\\u005Cbackslashes")

  , (Right [("doubleu_uu0020", "")], "doubleu_\\uu0020 ")
  , (Right [("upperu_U0020", "")], "upperu_\\U0020 ")
  , (Right [("missing_4_digits_uX", "")], "missing_4_digits_\\uX ")
  , (Right [("missing_3_digits_uX2", "")], "missing_3_digits_\\uX2 ")
  , (Right [("missing_2_digits_u20X", "")], "missing_2_digits_\\u20X ")
  , (Right [("missing_1_digit_u020X", "")], "missing_1_digit_\\u020X ")
  ]

test_truth_beauty_1 = testParse (Right [("Truth", "Beauty")], "Truth = Beauty")
test_truth_beauty_2 = testParse (Right [("Truth", "Beauty")], " Truth:Beauty")
test_truth_beauty_3 = testParse (Right [("Truth", "Beauty")], "Truth                    :Beauty")

test_fruits = testParse
  ( Right [("fruits", "apple, banana, pear, cantaloupe, watermelon, kiwi, mango")]
  , Text.unlines
    [ "fruits                           apple, banana, pear, \\"
    , "                                 cantaloupe, watermelon, \\"
    , "                                 kiwi, mango"
    ]
  )


prop_parsing_never_fails input
  = isRight $ parseProperties sourceName input
