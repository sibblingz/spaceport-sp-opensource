module Development.Spaceport.IOS.Settings
  ( identityFromString
  , identityToString
  ) where

import Control.Applicative
import Control.Monad.IO.Class
import Data.Maybe
import Development.IOS.Identity
import System.Directory
import System.FilePath
import System.IO.Prompt.Password
import System.Process.Runner

import Development.Spaceport.Tools.SpSigner
import Development.Spaceport.Util

import qualified Development.IOS.MobileProvision as Prov

identityFromString
  :: Runner IO SpSigner -> String -> IO Identity
identityFromString iOSSsigner ident
  = ifM (doesFileExist ident)
    (IdentityPKCS12File ident <$> p12Password iOSSsigner ident)
    (return . IdentityKeychain $ KeychainIdentity ident)

identityToString :: Identity -> String
identityToString (IdentityPKCS12File p12Path _) = p12Path
identityToString (IdentityKeychain (KeychainIdentity name)) = name

p12Password
  :: Runner IO SpSigner -> FilePath -> IO (Maybe Password)
p12Password signer p12Path
  = ifM (doesFileExist p12PasswordPath)
    (return . Just $ readPasswordFromFile p12PasswordPath)
    $ ifM (liftIO $ Prov.p12NeedsPassword signer p12Path)
      (return $ Just getPassword)
      (return Nothing)
  where
    p12PasswordPath = p12Path <.> "password"
    getPassword
      = fromMaybe (fail "Invalid identity password")
      <$> promptPassword "Password for identity? "
        (\ pass -> fmap (const pass)
          <$> Prov.verifyPKCS12 signer p12Path pass)
