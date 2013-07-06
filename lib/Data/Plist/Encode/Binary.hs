{-# LANGUAGE BangPatterns #-}

module Data.Plist.Encode.Binary
  ( toBinary

  , flattenValue
  , Value'(..)
  ) where

import Control.Monad
import Control.Monad.Trans.State.Strict
import Data.Plist.Types
import Data.Binary.IEEE754
import Data.Binary.Put
import Data.Bits
import Data.IntMap (IntMap)
import Data.Text (Text)
import Data.Word
import Control.Applicative

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSChar8
import qualified Data.ByteString.Lazy as BSLazy
import qualified Data.DList as DList
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Vector as V

type ValueIndex = Int
type Length = Int

data Value'
  = String' !Text
  | Real' !Double
  | Integer' !Integer !Length
  | Boolean' !Bool
  | Data' !BS.ByteString
  | Array' [ValueIndex]
  | Dictionary' (IntMap ValueIndex)
  deriving (Eq, Show)

data EncodeState = EncodeState
  { values :: DList.DList Value'
  , curIndex :: !ValueIndex
  -- | ^ Invariant: Equals the length of 'values'.
  , maxLength :: !Length
  }

initEncodingState :: EncodeState
initEncodingState = EncodeState DList.empty 0 0

toBinary :: Value -> BSLazy.ByteString
toBinary value = runPut $ do
  putByteString header
  writeValues serializedValues'
  writeOffsetTable offsetTable
  writeTrailer

  where
    writeValues = V.mapM_ putLazyByteString

    writeOffsetTable = V.mapM_
      $ \ x -> writeNumberLength
        (fromIntegral offsetSize)
        (fromIntegral x)

    writeTrailer = do
      replicateM_ 6 $ putWord8 0x00
      putWord8 $ fromIntegral offsetSize
      putWord8 $ fromIntegral indexSize

      putWord64be $ fromIntegral valueCount
      putWord64be $ fromIntegral topIndex
      putWord64be $ fromIntegral offsetTableOffset

    (values', _maxLen, topIndex) = flattenValue value
    valueCount = V.length values'
    indexSize = numberBytes valueCount

    serializeValues' = V.map (serializeValue' $ fromIntegral indexSize)
    serializedValues' = serializeValues' values'

    -- Each element in value'Offsets corresponds to the
    -- offset of serializedValues' in the output ByteString.
    value'Offsets :: V.Vector Int
    value'Offsets = V.scanl'
      (\ acc bs -> fromIntegral (BSLazy.length bs) + acc)
      initialOffset serializedValues'
    offsetTable = V.init value'Offsets
    offsetTableOffset = V.last value'Offsets
    offsetSize = numberBytes offsetTableOffset

    initialOffset = BS.length header

    header = BSChar8.pack "bplist00"

serializeValue' :: Length -> Value' -> BSLazy.ByteString
serializeValue' !indexLength !value = runPut $ case value of
  String' x -> do
    let bs = Text.encodeUtf16BE x
    let codeUnitCount = BS.length bs `div` 2
    writeDataHeader 0x6 $ fromIntegral codeUnitCount
    putByteString bs

  Real' x -> do
    putWord8 0x23
    putFloat64be x

  Integer' x _ -> do
    putWord8 0x13
    putWord64be $ fromIntegral x

  Boolean' True -> putWord8 0x09
  Boolean' False -> putWord8 0x08

  Data' x -> do
    writeDataHeader 0x4 . fromIntegral $ BS.length x
    putByteString x

  Array' xs -> do
    writeDataHeader 0xA . fromIntegral $ length xs
    writeIndices xs

  Dictionary' xs -> do
    writeDataHeader 0xD . fromIntegral $ IntMap.size xs
    writeIndices $ IntMap.keys xs
    writeIndices $ IntMap.elems xs

  where
    writeIndices = mapM_
      $ \ x -> writeNumberLength indexLength (fromIntegral x)

-- TODO Make generic.
writeDataHeader :: Word8 -> Integer -> Put
writeDataHeader objType len
  | len < 0 = fail "Negative length is invalid."
  | len < 0xF {-/= 0xF-} = putWord8 $ mkHeader (fromIntegral len)

  | len < 0x100 = writeWith 1
  | len < 0x10000 = writeWith 2
  | len < 0x100000000 = writeWith 3
  | otherwise = writeWith 4

  where
    mkHeader = ((objType `shiftL` 4) .|.)

    writeWith pot = do
      putWord8 $ mkHeader 0xF
      putWord8 $ pot + 0xF
      writeNumberPOT (fromIntegral pot) len

writeNumberPOT :: Int {-POT-} -> Integer -> Put
writeNumberPOT 1 = putWord8 . fromIntegral
writeNumberPOT 2 = putWord16be . fromIntegral
writeNumberPOT 3 = putWord32be . fromIntegral
writeNumberPOT !_ = putWord64be . fromIntegral

writeNumberLength :: Length -> Integer -> Put
writeNumberLength 1 = putWord8 . fromIntegral
writeNumberLength 2 = putWord16be . fromIntegral
writeNumberLength 3 = error "FIXME"  -- FIXME  -- putWord32be . fromIntegral
writeNumberLength 4 = putWord32be . fromIntegral
writeNumberLength !_ = putWord64be . fromIntegral

flattenValue :: Value -> (V.Vector Value', Length, ValueIndex)
flattenValue value
  = (V.fromListN valueCount valueList, maxLength encoding, valueIndex)
  where
    (valueIndex, encoding) = runState (indexValue value) initEncodingState
    valueList = DList.toList $ values encoding
    valueCount = curIndex encoding

-- | Number of bits required to store a given number.  At
-- least 1 to include a sign bit.
numberBits :: (Bits a, Integral a) => a -> Integer
numberBits = go 1
  where
    go c (-1) = c
    go c 0 = c
    go c n = go (succ c) $ n `unsafeShiftR` 1

-- | Number of bytes required to store a number of bits.
bitsToBytes :: (Integral a) => a -> a
bitsToBytes bits = (bits + 7) `div` 8

numberBytes :: (Bits a, Integral a) => a -> Integer
numberBytes = bitsToBytes . numberBits

putNewValue' :: Value' -> State EncodeState ValueIndex
putNewValue' v = do
  s <- get
  put $ s
    { values = values s `DList.snoc` v
    , curIndex = succ $ curIndex s
    , maxLength = max (maxLength s) $ case v of
      -- For String, we need the number of UTF-16 code
      -- units.  Let's be safe and assume Text is
      -- UTF-32-encoded.
      String' x -> Text.length x * 4
      Real' _ -> 0  -- FIXME
      Integer' _ len -> len
      Boolean' _ -> 1
      Data' x -> BS.length x
      Array' xs -> length xs
      Dictionary' xs -> IntMap.size xs * 2
    }
  return $ curIndex s

indexValue :: Value -> State EncodeState ValueIndex
indexValue (String x) = putNewValue' $ String' x
indexValue (Real x) = putNewValue' $ Real' x
indexValue (Integer x) = putNewValue' $ Integer' x (fromIntegral $ numberBytes x)
indexValue (Boolean x) = putNewValue' $ Boolean' x
indexValue (Data x) = putNewValue' $ Data' x

indexValue (Array xs) = putNewValue'
  =<< liftM Array' (mapM indexValue xs)

indexValue (Dictionary xs)
  = putNewValue'
  =<< liftM (Dictionary' . IntMap.fromList)
    (mapM putPair $ Map.toList xs)
  where
    putPair (k, v) = (,)
      <$> putNewValue' (String' k)
      <*> indexValue v
