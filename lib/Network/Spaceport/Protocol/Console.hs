{-# LANGUAGE CPP #-}

module Network.Spaceport.Protocol.Console
  ( Message
  , getMessage
  ) where

import Data.Text (Text)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSLazy
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text

import Network.Spaceport.Protocol.Internal

type Message = Text

lbsToStrict :: BSLazy.ByteString -> BS.ByteString
#if MIN_VERSION_bytestring(0, 10, 0)
lbsToStrict = BSLazy.toStrict
#else
lbsToStrict = BS.concat . BSLazy.toChunks
#endif

getMessage
  :: BSLazy.ByteString
  -> (Either Text.UnicodeException Message, BSLazy.ByteString)
getMessage input
  = (Text.decodeUtf8' $ lbsToStrict messageBytes, rest)
  where (messageBytes, rest) = getCommandChunk input
