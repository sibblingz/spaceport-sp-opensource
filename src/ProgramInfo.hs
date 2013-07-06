{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module ProgramInfo
  ( buildTimeStamp
  , buildContact
  , buildVersion
  ) where

import Data.Time.Clock
import Data.Text (Text)

import qualified Data.Text as Text

import TH

buildTimeStamp :: UTCTime
buildTimeStamp = $(getCurrentTimeQ)

buildContact :: Text
buildContact = Text.pack "support@spaceport.io"

buildVersion :: Text
#ifdef VERSION_process
buildVersion = Text.pack
  $ $(runCmd "git" ["rev-parse", "HEAD"] "(unknown)" (filter (/= '\n')))
#else
buildVersion = Text.pack "Generic"
#endif
