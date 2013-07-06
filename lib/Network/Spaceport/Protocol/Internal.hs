module Network.Spaceport.Protocol.Internal
  ( putCommandChunk
  , getCommandChunk

  , putCommandJSON
  , getCommandJSON
  ) where

import Data.Aeson
import Data.Binary

import qualified Data.ByteString.Lazy as BS

commandTerminator :: Word8
commandTerminator = 0x00

putCommandChunk :: BS.ByteString -> BS.ByteString
putCommandChunk bs
  = bs `BS.snoc` commandTerminator

getCommandChunk
  :: BS.ByteString
  -> (BS.ByteString, BS.ByteString)
getCommandChunk input
  = case BS.break (== commandTerminator) input of
    (front, back)
      | BS.null back -> (front, BS.empty)
      | otherwise -> (front, BS.tail back)

getCommandJSON
  :: (FromJSON a)
  => BS.ByteString
  -> (Maybe a, BS.ByteString)
getCommandJSON input
  = let (chunk, rest) = getCommandChunk input
    in (decode' chunk, rest)

putCommandJSON :: (ToJSON a) => a -> BS.ByteString
putCommandJSON = putCommandChunk . Data.Aeson.encode
