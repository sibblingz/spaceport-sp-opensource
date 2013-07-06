{-# LANGUAGE RecordWildCards #-}

module Development.Spaceport.Tools.SpSigner
  ( IdentitySource(..)
  , SpSigner(..)
  , spSignerClass
  , runSpSignerIO
  , runSpSignerAction
  ) where

import Control.Monad.IO.Class
import Data.Maybe
import Data.Text (Text)
import Development.IOS.Identity
import Development.Shake
import Development.Spaceport.Common.Tools
import System.IO (hPutStr)
import System.Java
import System.Process.Runner

import qualified Data.Text as Text

data IdentitySource
  = AppleKeychain
  | PKCS12File FilePath (Maybe Password)

data SpSigner
  -- | Signs an iOS .app directory in-place with the given
  -- credentials.
  = SignAppBundle
    { mobileProvisionFile :: FilePath
    , identity :: Identity
    , appBundle :: FilePath
    }
  | GetMobileProvisionInfo
    { mobileProvisionFile :: FilePath
    , outputAsJson :: Bool
    }
  | ListSigningIdentities
    { mMobileProvisionFile :: Maybe FilePath
    , identitySources :: [IdentitySource]
    }
  | VerifyIdentity
    { mobileProvisionFile :: FilePath
    , identity :: Identity
    }
  | SignJar
    { keystoreFile :: FilePath
    , keyAlias :: Text
    , keystorePassword :: Text
    , keyPassword :: Text
    , inputJar :: FilePath
    , outputJar :: FilePath
    }
  | GenerateKey
    { alias :: Text
    , description :: Text
    , validityDays :: Integer
    , keyPassword :: Text
    , keyStoreFile :: FilePath
    , keyStorePassword :: Text
    }

spSignerClass :: JavaClass
spSignerClass = "io.spaceport.iossigner.Main"

identityParameter :: Identity -> IO Parameter
identityParameter identity = case identity of
  IdentityKeychain (KeychainIdentity name)
    -> return $ option "--apple-keychain" name
  IdentityPKCS12File p12File (Just getPassword) -> do
    password <- liftIO getPassword
    return ["--pkcs12-pw", p12File, password]
  IdentityPKCS12File p12File Nothing
    -> return $ option "--pkcs12" p12File

identitySourceParameter :: IdentitySource -> IO Parameter
identitySourceParameter source = case source of
  AppleKeychain -> return $ flag "--apple-keychain"
  PKCS12File p12File (Just passwordIO) -> do
    password <- passwordIO
    return ["--pkcs12-pw", p12File, password]
  PKCS12File p12File Nothing -> return $ option "--pkcs12" p12File

needIdentity :: Identity -> Action ()
needIdentity (IdentityPKCS12File p12File _) = need [p12File]
needIdentity (IdentityKeychain _) = return ()

needIdentitySource :: IdentitySource -> Action ()
needIdentitySource source = case source of
  AppleKeychain -> return ()
  PKCS12File p12File _ -> need [p12File]

runSpSignerIO :: ExeRunner IO -> Runner IO SpSigner
runSpSignerIO = runSpSignerIO'

runSpSignerIO' :: (MonadIO m) => ExeRunner m -> Runner m SpSigner
runSpSignerIO' run signer = case signer of
  SignAppBundle {..} -> do
    identityParam <- liftIO $ identityParameter identity
    run $ concat
      [ arg "sign-bundle"
      , arg appBundle
      , option "--mprovision" mobileProvisionFile
      , identityParam
      ]

  GetMobileProvisionInfo {..} -> run $ concat
    [ arg "info"
    , flagIf outputAsJson "--json"
    , arg mobileProvisionFile
    ]

  ListSigningIdentities {..} -> do
    identitySourceParams <- liftIO
      $ mapM identitySourceParameter identitySources
    run $ concat
      $ [ arg "list"
        , maybeOption "--mprovision" mMobileProvisionFile
        ] ++ identitySourceParams

  VerifyIdentity {..} -> do
    identityParam <- liftIO $ identityParameter identity
    run $ concat
      [ arg "verify"
      , option "--mprovision" mobileProvisionFile
      , identityParam
      ]

  SignJar {..} -> do
    proc@(procStdin, _procStdout, _procStderr, _pid)
      <- run $ concat
        [ flag "jarsigner"
        , option "-sigalg" "MD5withRSA"  -- See note [Android signing].
        , option "-digestalg" "SHA1"
        , option "-keystore" keystoreFile
        , option "-signedjar" outputJar
        , arg inputJar
        , arg $ Text.unpack keyAlias
        ]
    liftIO $ hPutStr procStdin . Text.unpack $ Text.unlines
      [ keystorePassword
      , keyPassword
      ]
    return proc

  GenerateKey {..} -> run $ concat
    [ flag "keytool"
    , flag "-genkey"
    , option "-alias" $ Text.unpack alias
    , option "-keyalg" "RSA"
    , option "-dname" $ Text.unpack description
    , option "-validity" $ show validityDays
    -- FIXME Insecure!
    , option "-keypass" $ Text.unpack keyPassword
    , option "-keystore" keyStoreFile
    -- FIXME Insecure!
    , option "-storepass" $ Text.unpack keyStorePassword
    ]

needSpSigner :: SpSigner -> Action ()
needSpSigner signer = case signer of
  SignAppBundle {..} -> do
    need [mobileProvisionFile]  -- TODO appBundle?
    needIdentity identity
  GetMobileProvisionInfo {..}
    -> need [mobileProvisionFile]
  ListSigningIdentities {..} -> do
    need $ maybeToList mMobileProvisionFile
    mapM_ needIdentitySource identitySources
  VerifyIdentity {..} -> do
    need [mobileProvisionFile]
    needIdentity identity
  SignJar {..} -> need [inputJar, keystoreFile]
  GenerateKey {..} -> return ()

runSpSignerAction :: ExeRunner Action -> Runner Action SpSigner
runSpSignerAction run signer = do
  needSpSigner signer
  runSpSignerIO' run signer

-- Note [Android signing]:
--
-- The signature algorithm recommended by the Android
-- documentation[1] is MD5 with RSA; likewise, the
-- recommended digest algorithm is SHA1. Supported signature
-- and digest algorithms can be found in the "Java
-- Cryptography Architecture Standard Algorithm Name
-- Documentation"[2].
--
-- [1]: http://developer.android.com/tools/publishing/app-signing.html
-- [2]: http://download.java.net/jdk7/archive/b123/docs/technotes/guides/security/StandardNames.html
