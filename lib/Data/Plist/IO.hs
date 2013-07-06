module Data.Plist.IO
  ( readXML
  , writeXML

  , readBinary
  , writeBinary
  ) where

import Control.Exception
import Data.Conduit
import Data.Conduit.Binary
import Data.Plist.Decode
import Data.Plist.Encode
import Data.Plist.Types

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSLazy
import qualified Text.XML as X

readXML :: FilePath -> IO Value
readXML path = do
  contents <- BSLazy.readFile path
  case X.parseLBS X.def contents of
    Left err -> throwIO err
    Right xml -> case fromXMLDocument xml of
      Left err -> fail err
      Right value -> return value

writeXML :: FilePath -> Value -> IO ()
writeXML path value
  = runResourceT $ X.renderBytes settings xml $$ sinkFile path
  where
    settings = X.def { X.rsPretty = True }
    xml = toXMLDocument value

readBinary :: FilePath -> IO Value
readBinary path = do
  contents <- BS.readFile path
  either fail return $ fromBinary contents

writeBinary :: FilePath -> Value -> IO ()
writeBinary path = BSLazy.writeFile path . toBinary
