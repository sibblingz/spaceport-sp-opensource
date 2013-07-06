{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Data.IOS.BundleID where

import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit
import Test.Framework.TH
import Test.HUnit

import Data.IOS.BundleID

main = defaultMain tests
tests = [$(testGroupGenerator)]

isRight :: Either e a -> Bool
isRight (Right _) = True
isRight _ = False

case_empty
  = fromText ""
  @?= Left [EmptyInput]

case_one_word
  = isRight (fromText "hello") @?= True

case_two_words
  = isRight (fromText "hello.world") @?= True

case_adjacent_dots
  = fromText "hello..world"
  @?= Left [EmptyComponent]

case_space_at_end
  = fromText "hello.world "
  @?= Left [DisallowedCharacter ' ']

case_hyphen
  = isRight (fromText "com.company-name.app-id") @?= True

case_keyword
  = isRight (fromText "my.super.duper.game") @?= True

case_legal_numbers
  = isRight (fromText "over.nine1000") @?= True

case_initial_digits
  = isRight (fromText "over.9000") @?= True
