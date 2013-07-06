{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Data.Plist.EncodeDecode where

import Control.Monad
import Test.Framework (defaultMain)
import Test.Framework.Providers.QuickCheck2
import Test.Framework.TH
import Test.QuickCheck
import Test.QuickCheck.Instances ()

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSLazy
import qualified Data.Text as Text
import qualified Data.Text.Read as TextRead

import Data.Plist.Decode
import Data.Plist.Encode
import Data.Plist.Types

-- | Halves the size of the generator.  Useful to prevent
-- very large data structure generation.
halfSize :: Gen a -> Gen a
halfSize m = sized makeHalf
  where makeHalf size = resize (succ (size `div` 2)) m

-- | A double is reasonable if it can be stringified then
-- parsed without loss of precision.
isReasonableDouble :: Double -> Bool
isReasonableDouble x
  = check TextRead.double && check TextRead.rational
  where check f = f (Text.pack $ show x) == Right (x, Text.empty)

instance Arbitrary Value where
  arbitrary = do 
    x <- choose (0 :: Int, 6)
    case x of
      0 -> liftM String arbitrary
      1 -> liftM Real $ arbitrary `suchThat` isReasonableDouble
      2 -> liftM Integer arbitrary
      3 -> liftM Boolean arbitrary
      4 -> liftM Data arbitrary
      5 -> halfSize $ liftM Array arbitrary
      6 -> halfSize $ liftM Dictionary arbitrary
      _ -> error "FATAL ERROR: Arbitrary instance, logic bug"

main = defaultMain tests
tests = [$(testGroupGenerator)]

prop_plistToFromXMLDocument plistValue
  = Right plistValue == toFrom plistValue
  where
    toFrom = fromXMLDocument . toXMLDocument

prop_plistToFromBinary plistValue
  = Right plistValue == toFrom plistValue
  where
    toFrom = fromBinary . BS.concat . BSLazy.toChunks . toBinary

toFromBinary = fromBinary . BS.concat . BSLazy.toChunks . toBinary
