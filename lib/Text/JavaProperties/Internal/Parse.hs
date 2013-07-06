-- | Informal language specification is available in the
-- Java documentation of java.util.Properties:
-- http://docs.oracle.com/javase/7/docs/api/java/util/Properties.html#load(java.io.Reader)

{-# LANGUAGE FlexibleContexts #-}

module Text.JavaProperties.Internal.Parse
  ( parseProperties
  , parseProperties'
  ) where

import Control.Applicative hiding ((<|>), many, optional)
import Control.Monad
import Data.Maybe
import Data.Text (Text)
import Numeric (readHex)
import Text.Parsec
import Text.Parsec.Text ()

import qualified Data.Text as Text

import Text.JavaProperties.Internal.Types

eol :: (Stream s m Char) => ParsecT s u m ()
eol = try $ choice
  [ void $ char '\r' *> optional (char '\n')
  , void $ char '\n'
  ]

eolOrEOF :: (Stream s m Char) => ParsecT s u m ()
eolOrEOF = eol <|> eof

ws :: (Stream s m Char) => ParsecT s u m ()
ws = void $ oneOf " \t\f"

wsChars, eolChars, separatorChars :: [Char]
wsChars = " \t\f"
eolChars = "\r\n"
separatorChars = wsChars ++ ":="

separator :: (Stream s m Char) => ParsecT s u m ()
separator = choice
  [ void (oneOf "=:")
  , skipWS *> optional (oneOf "=:")
  ]

skipWS :: (Stream s m Char) => ParsecT s u m ()
skipWS = skipMany ws

skipWSNLComment :: (Stream s m Char) => ParsecT s u m ()
skipWSNLComment = skipMany $ ws <|> eol <|> comment

skipManyTill
  :: (Stream s m Char)
  => ParsecT s u m a
  -> ParsecT s u m ()
skipManyTill = void . manyTill anyChar

comment :: (Stream s m Char) => ParsecT s u m ()
comment = oneOf "#!" *> skipManyTill eolOrEOF

escapedChar :: (Stream s m Char) => ParsecT s u m Char
escapedChar = char '\\' *> do
  mChar <- choice [Just <$> anyChar, Nothing <$ eof]
  case mChar of
    Just 't' -> return '\t'
    Just 'n' -> return '\n'
    Just 'r' -> return '\r'

    Just 'u' -> try unicodeEscape <|> return 'u'
      where
        unicodeEscape = do
          digits <- replicateM 4 $ oneOf "0123456789abcdefABCDEF"
          let [(num, _)] = readHex digits
          return (toEnum num :: Char)

    Just c -> return c

    -- Backslash followed by EOF.
    Nothing -> return '\\'

charExcept
  :: (Stream s m Char)
  => [Char]
  -> ParsecT s u m Text
charExcept except = Text.pack . maybeToList <$> choice
  [ try lineContinuation
  , Just <$> (escapedChar <|> noneOf except)
  ] <?> "character"
  where
    lineContinuation = Nothing <$ char '\\' <* eol <* skipWS
      <?> "line continuation"

key :: (Stream s m Char) => ParsecT s u m Key
key = Text.concat <$> ((:) <$> keyCharInit <*> many keyCharRest)
  where
    keyCharInit = charExcept eolChars
      <?> keyCharMsg
    keyCharRest = charExcept (separatorChars ++ eolChars)
      <?> keyCharMsg
    keyCharMsg = "key character"

value :: (Stream s m Char) => ParsecT s u m Value
value = Text.concat <$> many valueChar
  where
    valueChar = charExcept eolChars
      <?> "value character"

entry :: (Stream s m Char) => ParsecT s Text m Entry
entry = Entry
  <$> key
  <*> optionMaybe (separator *> skipWS *> located value)
  <* skipWS <* eolOrEOF

offset :: (Stream s m t) => ParsecT s Text m Offset
offset = (\ text pos -> fromJust $ charOffset text pos)
  <$> getState <*> getPosition

located
  :: (Stream s m t)
  => ParsecT s Text m a
  -> ParsecT s Text m (Located a)
located p = (\ start x end -> Located x (Range start end))
  <$> offset <*> p <*> offset

propertiesFile
  :: (Stream s m Char)
  => ParsecT s Text m [Located Entry]
propertiesFile
  = skipWSNLComment *> many (located entry <* skipWSNLComment)

parseProperties
  :: SourceName -> Text -> Either ParseError File
parseProperties source input = File input endl
  <$> runParser propertiesFile input source input
  where endl = LF  -- TODO

parseProperties' :: Text -> File
parseProperties' input
  = either err id $ parseProperties source input
  where
    err e = error $ "Text.JavaProperties.Parse.parseProperties': "
      ++ show e
    source = ""

charOffset :: Text -> SourcePos -> Maybe Offset
charOffset text pos
  = join . eitherToMaybe
  $ parse (parser 0) "" text
  where
    eitherToMaybe = either (const Nothing) Just
    parser offs = do
      curPos <- getPosition
      if curPos == pos
        then return (Just offs)
        else (anyChar >> parser (succ offs))
          <|> return Nothing
