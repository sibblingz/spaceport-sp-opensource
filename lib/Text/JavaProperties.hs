module Text.JavaProperties
  ( File
  , Key
  , Value

  , parseProperties
  , parseProperties'
  , fileContents
  , emptyFile

  , filePairs
  , setValue
  , deleteKey

  , mapPropertiesFile
  , readPropertiesFile
  , readPropertiesFileOrEmpty
  ) where

import Control.Applicative
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import Data.Text.Edit
import System.Directory
import System.FilePath

import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Development.Spaceport.Util
import Text.JavaProperties.Internal.Parse
import Text.JavaProperties.Internal.Print
import Text.JavaProperties.Internal.Types

-- | Gets all key-value pairs defined by a Java properties
-- file.
filePairs :: File -> [(Key, Value)]
filePairs (File _ _ entries)
  = map (getPair . unlocated) entries
  where
    getPair (Entry k v) = (k, maybe Text.empty unlocated v)

trailingBackslashes :: Text -> Int
trailingBackslashes
  = Text.length . Text.takeWhile (== '\\')
  . Text.reverse

-- | Appends line endings.  Inserts enough line endings to
-- ensure a blank line between the previous text.  Also
-- preserves backslashes before the end of a string.
addEndl :: LineEnding -> Text -> Text
addEndl endl text
  | Text.null text = Text.empty
  | Text.last text == '\r' = text <> eolAfterCR
  | Text.last text == '\n' = text <> eolAfterLF
  | odd (trailingBackslashes text) = text `Text.snoc` '\\' <> eol <> eol
  | otherwise = text <> eol <> eol
  where
    eol = lineEndingText endl
    eolAfterLF = eol

    -- | An LF after a CR is recognized as a CRLF, which
    -- converts a newline instead of appending one.
    eolAfterCR = lineEndingText $ case endl of
      CR -> LF
      LF -> CRLF
      CRLF -> CRLF

data EntryEdit
  = Delete
  | Replace Key (Maybe Value)
  | SetValue Value

getTextChange
  :: LineEnding
  -> Located Entry
  -> EntryEdit
  -> Change
getTextChange endl (Located entry (Range start end)) edit = case edit of
  Delete -> mkChange start end Text.empty

  Replace key mValue -> case mValue of
    Nothing -> mkChange start end entryText
      where entryText = makeKeyEntry endl key

    Just value -> mkChange start end entryText
      where entryText = fst $ makeKeyValueEntry endl key value

  SetValue value -> case entry of
    Entry _ (Just (Located _ (Range vStart vEnd)))
      -> mkChange vStart vEnd $ escapeValue value

    Entry _ Nothing
      -> error "Text.JavaProperties.editFileEntries: TODO"

editFileEntries
  :: (Entry -> Maybe EntryEdit)
  -> File
  -> File
editFileEntries f (File contents endl entries)
  = parseProperties' $ applyChanges changes contents
  where
    changes = mapMaybe getChange entries
    getChange entry 
      = getTextChange endl entry <$> f (unlocated entry)

-- | Sets the value of a key, replacing an existing value if
-- present or appending a new key-value pair otherwise.
--
-- If multiple values exist for a given key, all key-value
-- pairs are removed first, and the new key-value pair is
-- appended.
setValue :: Key -> Value -> File -> File
setValue key value file@(File _ _ entries) = case entriesWithKey of
  -- Two or more entries: delete and append.
  (_:_:_) -> addValue key value $ editFileEntries (deleteIfKey key) file

  -- One entry: replace.
  [_] -> editFileEntries replaceValueIfKey file

  -- No entries: append.
  [] -> addValue key value file

  where
    entriesWithKey = filter (hasKey key . unlocated) entries

    replaceValueIfKey e
      | hasKey key e = Just $ SetValue value
      | otherwise = Nothing

hasKey :: Key -> Entry -> Bool
hasKey key entry = key == entryKey entry

deleteIfKey :: Key -> Entry -> Maybe EntryEdit
deleteIfKey key e
  | hasKey key e = Just Delete
  | otherwise = Nothing

-- | Appends a key-value pair to the end of a file.
addValue :: Key -> Value -> File -> File
addValue key value (File contents endl _)
  = parseProperties'
  $ addEndl endl contents
    <> fst (makeKeyValueEntry endl key value)

-- | Unsets the value of a key by removing all corresponding
-- key-value pairs.  If the key-value pair does not exist,
-- nothing is changed.
deleteKey :: Key -> File -> File
deleteKey key file
 = editFileEntries (deleteIfKey key) file

-- | Edits a properties file in-place.
mapPropertiesFile
  :: (File -> File)
  -> FilePath -> IO ()
mapPropertiesFile f path = do
  createDirectoryIfMissing True (dropFileName path)
  Text.writeFile path . fileContents . f
    =<< readPropertiesFileOrEmpty path

readPropertiesFile :: FilePath -> IO File
readPropertiesFile = fmap parseProperties' . Text.readFile

readPropertiesFileOrEmpty :: FilePath -> IO File
readPropertiesFileOrEmpty path = ifM (doesFileExist path)
  (readPropertiesFile path)
  (return emptyFile)
