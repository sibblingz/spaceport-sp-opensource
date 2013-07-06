{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module QuasiQuoters
  ( byteString
  , heredoc
  , text
  , xml
  ) where

import Data.Conduit
import Data.Conduit.Binary
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import System.IO.Unsafe

import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextLazy
import qualified Data.Text.Lazy.Encoding as TextLazy
import qualified Text.XML as X

heredoc :: QuasiQuoter
heredoc = QuasiQuoter
  { quoteExp = stringE
  , quotePat = error "heredoc: quotePat"
  , quoteType = error "heredoc: quoteType"
  , quoteDec = error "heredoc: quoteDec"
  }

parseXML :: TextLazy.Text -> X.Document
parseXML txt
  = unsafePerformIO . runResourceT
  $ source $$ X.sinkDoc X.def
  where
    source = sourceLbs $ TextLazy.encodeUtf8 txt

quoteXMLExp :: String -> ExpQ
quoteXMLExp s = [| parseXML $ TextLazy.pack s |]

xml :: QuasiQuoter
xml = QuasiQuoter
  { quoteExp = quoteXMLExp
  , quotePat = error "xml: quotePat: Unsupported"
  , quoteType = error "xml: quoteType: Unsupported"
  , quoteDec = error "xml: quoteDec: Unsupported"
  }

quoteByteStringExp :: String -> ExpQ
quoteByteStringExp s = [| TextLazy.encodeUtf8 $ TextLazy.pack s |]

byteString :: QuasiQuoter
byteString = QuasiQuoter
  { quoteExp = quoteByteStringExp
  , quotePat = error "byteString: quotePat: Unsupported"
  , quoteType = error "byteString: quoteType: Unsupported"
  , quoteDec = error "byteString: quoteDec: Unsupported"
  }

quoteTextExp :: String -> ExpQ
quoteTextExp s = [| Text.pack s |]

text :: QuasiQuoter
text = QuasiQuoter
  { quoteExp = quoteTextExp
  , quotePat = error "text: quotePat: Unsupported"
  , quoteType = error "text: quoteType: Unsupported"
  , quoteDec = error "text: quoteDec: Unsupported"
  }
