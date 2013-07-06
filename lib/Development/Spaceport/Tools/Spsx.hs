{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module Development.Spaceport.Tools.Spsx
  ( Spsx(..)
  , runSpsxAction
  ) where

import Development.Shake
import System.Process.Runner

data Spsx = Spsx
  { inputMp3 :: FilePath
  , outputOgg :: FilePath
  , quality :: Double
  }

runSpsxAction :: ExeRunner Action -> Runner Action Spsx
runSpsxAction run spsx@Spsx {..} = do
  need [inputMp3]
  run $ spsxOptions spsx

spsxOptions :: Spsx -> [String]
spsxOptions Spsx {..}
  -- HACK; Linux version doesn't have -i, -o, -q; OS X
  -- version does.
#ifdef linux_HOST_OS
  = [inputMp3, outputOgg, show quality]
#else
  = [ "-i", inputMp3
    , "-o", outputOgg
    , "-q", show quality
    ]
#endif
