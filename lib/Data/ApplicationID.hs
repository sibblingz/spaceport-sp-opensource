module Data.ApplicationID
  ( ApplicationID
  , toText
  , fromText
  , toPackageName
  , toBundleID
  ) where

import Data.Android.PackageName (PackageName)
import Data.Function (on)
import Data.IOS.BundleID (BundleID)
import Data.Text (Text)

import qualified Data.Android.PackageName as PackageName
import qualified Data.IOS.BundleID as BundleID
import qualified Data.List
import qualified Data.Text as Text

data ApplicationID
  = ApplicationID Text PackageName BundleID

instance Eq ApplicationID where
  (==) = (==) `on` toText

instance Ord ApplicationID where
  compare = compare `on` toText

instance Show ApplicationID where
  show = show . toText

toText :: ApplicationID -> Text
toText (ApplicationID text _ _) = text

data ParseError
  = DisallowedComponent Text
  | DisallowedCharacter Char
  | EmptyComponent
  | NeedAtLeastTwoComponents
  | EmptyInput
  deriving (Eq)

instance Show ParseError where
  show (DisallowedComponent word)
    = "'" ++ Text.unpack word ++ "' is not allowed"
  show (DisallowedCharacter c)
    = show c ++ " is not an allowed character"
  show EmptyComponent = "empty components are not allowed"
  show NeedAtLeastTwoComponents = "at least one '.' is required"
  show EmptyInput = "input is empty"

fromText :: Text -> Either [ParseError] ApplicationID
fromText text
  = case (PackageName.fromText text, BundleID.fromText text) of
    (Right packageName, Right bundleID) -> Right
      $ ApplicationID text packageName bundleID
    (badPackageName, badBundleID) -> Left
      . Data.List.nub $ packageNameErrs ++ bundleIDErrs
      where
        packageNameErrs = either (map fromPackageNameParseError)
          (const []) badPackageName
        bundleIDErrs = either (map fromBundleIDParseError)
          (const []) badBundleID

fromPackageNameParseError :: PackageName.ParseError -> ParseError
fromPackageNameParseError err = case err of
  PackageName.DisallowedComponent x -> DisallowedComponent x
  PackageName.DisallowedCharacter x -> DisallowedCharacter x
  PackageName.EmptyComponent -> EmptyComponent
  PackageName.NeedAtLeastTwoComponents -> NeedAtLeastTwoComponents
  PackageName.EmptyInput -> EmptyInput

fromBundleIDParseError :: BundleID.ParseError -> ParseError
fromBundleIDParseError err = case err of
  BundleID.DisallowedCharacter x -> DisallowedCharacter x
  BundleID.EmptyComponent -> EmptyComponent
  BundleID.EmptyInput -> EmptyInput

toPackageName
  :: ApplicationID -> PackageName
toPackageName
  (ApplicationID _ x _) = x

toBundleID
  :: ApplicationID -> BundleID
toBundleID
  (ApplicationID _ _ x) = x
