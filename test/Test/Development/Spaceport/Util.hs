{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Development.Spaceport.Util where

import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit
import Test.Framework.TH
import Test.HUnit

import Development.Spaceport.Util

main = defaultMain tests
tests = [$(testGroupGenerator)]

case_readHumanBool_t    = Just True @=? readHumanBool "t"
case_readHumanBool_true = Just True @=? readHumanBool "true"
case_readHumanBool_1    = Just True @=? readHumanBool "1"
case_readHumanBool_y    = Just True @=? readHumanBool "y"
case_readHumanBool_Yes  = Just True @=? readHumanBool "Yes"

case_readHumanBool_f     = Just False @=? readHumanBool "f"
case_readHumanBool_false = Just False @=? readHumanBool "false"
case_readHumanBool_0     = Just False @=? readHumanBool "0"
case_readHumanBool_n     = Just False @=? readHumanBool "n"
case_readHumanBool_No    = Just False @=? readHumanBool "No"

case_ioLines_unix
  = ["these", "are", "", "lines"]
  @=? ioLines "these\nare\n\nlines\n"
case_ioLines_windows
  = ["these", "are", "", "lines"]
  @=? ioLines "these\r\nare\r\n\r\nlines\r\n"
case_ioLines_adbWindows
  = ["these", "are", "", "lines"]
  @=? ioLines "these\r\r\nare\r\r\n\r\r\nlines\r\r\n"
case_ioLines_adbShell
  = ["GT-I9300"]
  @=? ioLines "GT-I9300\r\r\n"
case_ioLines_adbInstall
  = ["\tpkg: /data/local/tmp/built.apk","Success"]
  @=? ioLines "\tpkg: /data/local/tmp/built.apk\r\r\nSuccess\r\r\n"
