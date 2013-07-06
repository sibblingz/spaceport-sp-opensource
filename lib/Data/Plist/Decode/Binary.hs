{-# LANGUAGE OverloadedStrings #-}

module Data.Plist.Decode.Binary
  ( fromBinary

  , readIntegerSignedBE
  , readIntegerUnsignedBE
  ) where

import Control.Applicative
import Control.Exception as Ex
import Control.Monad
import Data.Attoparsec as Atto
import Data.Attoparsec.Binary
import Data.Binary.Get
import Data.Binary.IEEE754
import Data.Bits
import Data.Plist.Types
import Data.Text (Text)
import Data.Word
import System.IO.Unsafe

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSLazy
import qualified Data.Map as Map
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import qualified Data.Vector.Unboxed as V

fromBinary :: BS.ByteString -> Either String Value
fromBinary input = do
  when (BS.null bodyBS || BS.length trailerBS /= trailerSize)
    $ Left "Invalid binary plist size."

  void $ parseOnly (string "bplist") input

  trailer <- parseTrailer trailerBS
  parseBinary trailer bodyBS

  where
    (bodyBS, trailerBS) = BS.splitAt (BS.length input - trailerSize) input

trailerSize :: Int
trailerSize = 0x20

data Trailer = Trailer
  { trailerOffsetSize :: !Word8
  , trailerRefSize :: !Word8
  , trailerNumObjects :: !Word64
  , trailerTopObject :: !Word64
  , trailerOffsetTableOffset :: !Word64
  }
  deriving (Show)

parseTrailer :: BS.ByteString -> Either String Trailer
parseTrailer = parseOnly $ Trailer
  <$ replicateM 6 (void anyWord8)
  <*> anyWord8
  <*> anyWord8
  <*> anyWord64be
  <*> anyWord64be
  <*> anyWord64be
  <* endOfInput

readIntegerSignedBE :: BS.ByteString -> Integer
readIntegerSignedBE input
  | isNegative = unsigned - maxValue
  | otherwise = unsigned
  where
    isNegative = testBit (BS.last input) 7
    unsigned = BS.foldl'
      (\ acc x -> acc `shiftL` 8 .|. fromIntegral x)
      0 input

    maxValue = 1 `shiftL` (BS.length input * 8)

readIntegerUnsignedBE :: BS.ByteString -> Integer
readIntegerUnsignedBE = BS.foldl'
  (\ acc x -> acc `shiftL` 8 .|. fromIntegral x) 0

parseIntegerUnsignedBE :: Word8 -> Parser Integer
parseIntegerUnsignedBE
  = liftM readIntegerUnsignedBE
  . Atto.take . fromIntegral

parseIntegerSignedBE :: Word8 -> Parser Integer
parseIntegerSignedBE
  = liftM readIntegerSignedBE
  . Atto.take . fromIntegral

parseDoubleBE :: Word8 -> Parser Double
parseDoubleBE len = do
  lazyBytes <- Atto.take $ fromIntegral len
  let bytes = BSLazy.fromChunks [lazyBytes]
  let p fn = return . realToFrac $ runGet fn bytes
  case len of
    2 -> p getFloat16be
    4 -> p getFloat32be
    8 -> p getFloat64be
    _ -> fail "Unknown real number size."

decodeUtf16BE' :: BS.ByteString -> Either Text.UnicodeException Text
decodeUtf16BE'
  = unsafePerformIO . Ex.try . evaluate
  . Text.decodeUtf16BE

-- Reference:
-- http://code.google.com/p/plist/source/browse/trunk/src/main/java/com/dd/plist/BinaryPropertyListParser.java
parseBinary :: Trailer -> BS.ByteString -> Either String Value
parseBinary trailer input = do
  offsets <- parseOffsetTable
    $ BS.drop (fromIntegral $ trailerOffsetTableOffset trailer) input
  parseBinary' trailer (V.fromList $ map fromIntegral offsets) input

  where
    parseOffsetTable = parseOnly
      . replicateM64 (trailerNumObjects trailer)
      $ parseIntegerUnsignedBE (trailerOffsetSize trailer)

parseBinary'
  :: Trailer
  -> V.Vector Word64
  -> BS.ByteString
  -> Either String Value
parseBinary' trailer offsetTable input
  = readObjectAtIndex $ trailerTopObject trailer
  where
    -- | Reads an object at the given index in the offset
    -- table.
    readObjectAtIndex :: Word64 -> Either String Value
    readObjectAtIndex index
      = case offsetTable V.!? fromIntegral index of
        Just offset -> readObject offset
        Nothing -> Left "Invalid object index."

    -- | Reads an object at the given byte offset.
    readObject :: Word64 -> Either String Value
    readObject = parseOnly parseObject . fromOffset
    fromOffset offset = BS.drop (fromIntegral offset) input

    parseObject :: Parser Value
    parseObject = do
      info <- anyWord8
      let objData = (info `shiftR` 0) .&. 0x0F
      let objType = (info `shiftR` 4) .&. 0x0F

      case objType of
        0x0 -> case objData of
          0x0 -> fail "Null not supported."  -- TODO/FIXME
          0x8 -> return $ Boolean False
          0x9 -> return $ Boolean True
          0xF -> fail "Filler byte not supported."  -- TODO/FIXME
          _ -> fail $ "Unknown unit type '" ++ show objData ++ "'."

        0x1 -> Integer <$> parseIntegerSignedBE (integerSize objData)
        0x2 -> Real <$> parseDoubleBE (integerSize objData)

        0x3 -> case objData of
          0x3 -> fail "TODO Parse date."  -- TODO
          _ -> fail "Unknown date type."

        0x4 -> liftM Data $ readData objData

        -- ASCII string (but we parse UTF-8 anyway).
        0x5 -> liftM String
          $ leftToFailShow . Text.decodeUtf8' =<< readData objData
        0x6 -> liftM String $ do
          size <- dataSize objData
          let byteCount = 2 * size
          leftToFailShow . decodeUtf16BE'
            =<< Atto.take (fromIntegral byteCount)

        0x8 -> fail "TODO Parse UID."  -- TODO

        0xA -> do
          len <- dataSize objData
          liftM Array $ replicateM64 len parseIndexedObject

        0xB -> fail "TODO Parse Set."  -- TODO

        0xD -> do
          len <- dataSize objData
          keys <- replicateM64 len parseIndexedObject
          values <- replicateM64 len parseIndexedObject

          stringKeys <- forM keys $ \ key -> case key of
            String text -> return text
            _ -> fail "Expected string for dictionary key."

          return . Dictionary . Map.fromList $ zip stringKeys values

        _ -> fail $ "Unknown object type '" ++ show objType ++ "'."

    integerSize objData = 1 `shiftL` fromIntegral objData

    parseIndexedObject :: Parser Value
    parseIndexedObject = parseIndex >>= leftToFail . readObjectAtIndex

    parseIndex :: Parser Word64
    parseIndex = fromIntegral
      <$> parseIntegerUnsignedBE (trailerRefSize trailer)

    readData :: Word8 -> Parser BS.ByteString
    readData objData = Atto.take . fromIntegral =<< dataSize objData

    dataSize :: Word8 -> Parser Word64
    dataSize 0xF = do
      info <- anyWord8
      let intData = (info `shiftR` 0) .&. 0x0F
      let intType = (info `shiftR` 4) .&. 0x0F
      case intType of
        0x1 -> fmap fromIntegral . parseIntegerUnsignedBE $ integerSize intData
        _ -> fail $ "Unknown data length type '" ++ show intType ++ "'."
    dataSize objData = return $ fromIntegral objData

    leftToFail (Left err) = fail err
    leftToFail (Right x) = return x

    leftToFailShow (Left err) = fail $ show err
    leftToFailShow (Right x) = return x

replicateM64 :: (Monad m) => Word64 -> m a -> m [a]
replicateM64 n = replicateM (fromIntegral n)
