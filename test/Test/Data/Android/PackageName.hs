{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Data.Android.PackageName where

import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit
import Test.Framework.TH
import Test.HUnit

import Data.Android.PackageName

main = defaultMain tests
tests = [$(testGroupGenerator)]

isRight :: Either e a -> Bool
isRight (Right _) = True
isRight _ = False

case_empty
  = fromText ""
  @?= Left [EmptyInput]

case_one_word
  = fromText "hello"
  @?= Left [NeedAtLeastTwoComponents]

case_two_words
  = isRight (fromText "hello.world") @?= True

case_adjacent_dots
  = fromText "hello..world"
  @?= Left [EmptyComponent]

case_space_at_end
  = fromText "hello.world "
  @?= Left [DisallowedCharacter ' ']

case_keyword
  = fromText "my.super.duper.game"
  @?= Left [DisallowedComponent "super"]

case_legal_numbers
  = isRight (fromText "over.nine1000") @?= True

case_illegal_numbers
  = fromText "over.9000"
  @?= Left [DisallowedCharacter '9']
