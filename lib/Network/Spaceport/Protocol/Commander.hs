{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Spaceport.Protocol.Commander
  ( SequenceID(..)

  , URL

  , Command(..)
  , CommandType(..)
  , putCommand

  , Response(..)
  , ResponseType(..)
  , getResponse
  ) where

import Control.Applicative
import Data.Aeson hiding (Success)
import Data.Aeson.Types hiding (Success)
import Data.Binary
import Data.Text (Text)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as Text
import qualified Data.HashMap.Strict as HMap

import Development.Spaceport.Util
import Network.Spaceport.Protocol.Internal

newtype SequenceID = SequenceID Word32
  deriving (ToJSON, Enum, Eq, Ord)

instance FromJSON SequenceID where
  parseJSON (String s) = case maybeRead $ Text.unpack s of
    Just num -> return $ SequenceID num
    Nothing -> fail "Invalid sequence ID."
  parseJSON x = SequenceID <$> parseJSON x

instance Show SequenceID where
  show (SequenceID seqID) = '@' : show seqID

type URL = String

-- | A 'CommandType' with a possibly-attached 'SequenceID'.
-- Responses may contain a 'SequenceID', meaning they are
-- responding to the command with the same 'SequenceID'.
data Command = Command CommandType (Maybe SequenceID)

instance Show Command where
  show (Command typ (Just seqID)) = unwords
    ["Command", show seqID, show typ]
  show (Command typ Nothing) = unwords
    ["Command", show typ]

data CommandType
  -- | Launches a URL on the Spaceport client.  The URL must
  -- point to an HTML resource or a manifest.xml resource.
  = Launch URL
  deriving (Show)

data Response = Response ResponseType (Maybe SequenceID)

instance Show Response where
  show (Response typ (Just seqID)) = unwords
    ["Response", show seqID, show typ]
  show (Response typ Nothing) = unwords
    ["Response", show typ]

data ResponseType
  = Success
  | Ready
  deriving (Show)

putCommand :: Command -> BS.ByteString
putCommand = putCommandJSON

instance FromJSON Command where
  parseJSON _ = error "TODO FromJSON Command"

instance ToJSON Command where
  toJSON (Command typ mSeqID) = case mSeqID of
    Just seqID -> object
      [name .= object ["cmd" .= seqID, "params" .= body]]

    Nothing -> object
      [name .= object ["params" .= body]]

    where
      body = case typ of
        Launch url -> object ["url" .= url]

      name = case typ of
        Launch _ -> "launch"

getResponse
  :: BS.ByteString
  -> (Maybe Response, BS.ByteString)
getResponse = getCommandJSON

readResponseType
  :: Text
  -> Maybe Value
  -> Parser ResponseType
readResponseType "ack" _ = return Success
readResponseType "ready" _ = return Ready
readResponseType _ _ = fail "Unknown response type."

instance FromJSON Response where
  parseJSON (Object obj) = case HMap.toList obj of
    [(k, Object v)] -> do
      results <- v .:? "results"
      Response
        <$> readResponseType k results
        <*> v .:? "cmd"
    _ -> fail "Malformed response."

  parseJSON _ = fail "Malformed response."

instance ToJSON Response where
  toJSON _ = error "TODO ToJSON Response"
