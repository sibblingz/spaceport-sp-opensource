{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Data.Text.Edit where

import Data.Text ()
import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.Framework.TH
import Test.HUnit
import Test.QuickCheck.Instances ()

import Data.Text.Edit

main = defaultMain tests
tests = [$(testGroupGenerator)]

prop_no_changes_noop text = applyChanges [] text == text

case_delete_beginning = applyChanges
  [ mkChange 0 2 "" ]
  "hello world"
  @?= "llo world"

case_delete_middle = applyChanges
  [ mkChange 4 8 "" ]
  "hello world"
  @?= "hellrld"

case_replace_2 = applyChanges
  [ mkChange 8 9 "hi"
  , mkChange 2 4 "or"
  ]
  "hello world"
  @?= "heoro wohild"

case_replace_2_r = applyChanges
  [ mkChange 2 4 "or"
  , mkChange 8 9 "hi"
  ]
  "hello world"
  @?= "heoro wohild"
