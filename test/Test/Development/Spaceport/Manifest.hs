{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Development.Spaceport.Manifest where

import Data.DeriveTH
import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.Framework.TH
import Test.HUnit
import Test.QuickCheck
import Test.QuickCheck.Instances ()

import qualified Text.XML as X
import qualified Text.XML.DOMExtras as X

import Development.Spaceport.Manifest
import QuasiQuoters

$(derive makeArbitrary ''ManifestFileEntry)
$(derive makeArbitrary ''ManifestFile)
$(derive makeArbitrary ''ManifestPlugin)
$(derive makeArbitrary ''CDNConfig)

main = defaultMain tests
tests = [$(testGroupGenerator)]

prop_manifestToFromXML manifestFile cdnConfig
  = Right (manifestFile, cdnInfo) == toFrom (manifestFile, cdnInfo)
  where
    toFrom = manifestFromXML . uncurry manifestToXML
    cdnInfo = manifestCDNInfo cdnConfig manifestFile

stripNodeRecursive :: X.Document -> X.Document
stripNodeRecursive = X.filterDocumentNodes f
  where
    f (X.NodeElement _) = True
    f (X.NodeInstruction _) = False
    f (X.NodeContent _) = False
    f (X.NodeComment _) = False

-- | Ensure generation of the CDN hash, key, and URL are
-- correct.
case_pluginless_cdn_hash_info
  = (actualManifestXML, actualCDNInfo)
  @?= (stripNodeRecursive manifestXML, expectedCDNInfo)

  where
    Right (expectedManifest, expectedCDNInfo) = manifestFromXML manifestXML

    actualManifestXML = manifestToXML expectedManifest actualCDNInfo
    actualCDNInfo = manifestCDNInfo cdnConfig expectedManifest

    cdnConfig = CDNConfig
      { cdnConfigAuthorizationKey
          = "kgnc2ki7omia1xcleop2b05megr9atwm3bxo8qtgipaz074x9uxmq7n4sxs5ivdz"
      , cdnConfigPackageName = "io.spaceport.mg.match3"
      }

    manifestXML
      = [xml|
        <?xml version="1.0" encoding="UTF-8"?>
        <manifest
          boot="index.html"
          cdn="http://cdn.spaceport.io/io.spaceport.mg.match3/rdyG4UVPUtoPTXMxwvWvkg/"
          description="this is a description"
          hash="rdyG4UVPUtoPTXMxwvWvkg=="
          key="+I97GziCOmGZU+h6oqyONw=="
          package="io.spaceport.mg.match3"
          platform="4.0"
          time="time_goes_here"
          url="http://apps.spaceport.io/io.spaceport.mg.match3/18KbAj5sQI0AIR_Q131s_w/manifest.xml"
          version="1.0">
            <plugins/>
            <startup>
                <entry
                  filesize="105420"
                  id="game.js"
                  version="8clufM3LjP6n/Z9+cWxFAQ=="/>
                <entry
                  filesize="948"
                  id="assets/Gems.sgf"
                  version="gE/aIrQEtS5nYY6B/cyCJA=="/>
                <entry
                  filesize="186"
                  id="swc_libraries/GemAssets.sgf"
                  version="5NoSx7utzIr4dlkyOlCrXA=="/>
                <entry
                  filesize="414"
                  id="index.html"
                  version="pI8UYgmPg9j7I55mJ2JbAQ=="/>
                <entry
                  filesize="69491"
                  id="spaceport/spaceport.js"
                  version="k4mKnn6KP8voaUqDHPYLbg=="/>
                <entry
                  filesize="8762"
                  id="spaceport/unrequire.js"
                  version="RlrgzBCZBQjLZ6/32M+gEA=="/>
            </startup>
        </manifest>
        |]

-- | Ensure generation of the CDN hash, key, and URL are
-- correct.
case_pluginfull_cdn_hash_info
  = (actualManifestXML, actualCDNInfo)
  @?= (stripNodeRecursive manifestXML, expectedCDNInfo)

  where
    Right (expectedManifest, expectedCDNInfo) = manifestFromXML manifestXML

    actualManifestXML = manifestToXML expectedManifest actualCDNInfo
    actualCDNInfo = manifestCDNInfo cdnConfig expectedManifest

    cdnConfig = CDNConfig
      { cdnConfigAuthorizationKey
          = "kgnc2ki7omia1xcleop2b05megr9atwm3bxo8qtgipaz074x9uxmq7n4sxs5ivdz"
      , cdnConfigPackageName = "io.spaceport.mg.match3"
      }

    manifestXML
      = [xml|
        <?xml version="1.0" encoding="UTF-8"?>
        <manifest
          boot="index.html"
          cdn="http://cdn.spaceport.io/io.spaceport.mg.match3/s6b+Fjb+Hs0yvEpsdZd7Ow/"
          hash="s6b+Fjb+Hs0yvEpsdZd7Ow=="
          key="mp4IJj0rk2tLWUrvHIYRPg=="
          package="io.spaceport.mg.match3"
          platform="4.0"
          time="time_goes_here"
          url="http://apps.spaceport.io/io.spaceport.mg.match3/6_fYpZVx9nYW4Hm60dpxpg/manifest.xml"
          version="1.0">
            <plugins>
                <entry id="io.spaceport.plugins.contacts.ContactsPlugin" version="4.0.0"/>
                <entry id="io.spaceport.plugins.mail.MailPlugin" version="4.0.0"/>
                <entry id="io.spaceport.plugins.sms.SmsPlugin" version="4.0.0"/>
            </plugins>
            <startup>
                <entry
                  filesize="105420"
                  id="game.js"
                  version="8clufM3LjP6n/Z9+cWxFAQ=="/>
                <entry
                  filesize="948"
                  id="assets/Gems.sgf"
                  version="gE/aIrQEtS5nYY6B/cyCJA=="/>
                <entry
                  filesize="186"
                  id="swc_libraries/GemAssets.sgf"
                  version="5NoSx7utzIr4dlkyOlCrXA=="/>
                <entry
                  filesize="414"
                  id="index.html"
                  version="pI8UYgmPg9j7I55mJ2JbAQ=="/>
                <entry
                  filesize="69491"
                  id="spaceport/spaceport.js"
                  version="k4mKnn6KP8voaUqDHPYLbg=="/>
                <entry
                  filesize="8762"
                  id="spaceport/unrequire.js"
                  version="RlrgzBCZBQjLZ6/32M+gEA=="/>
            </startup>
        </manifest>
        |]
