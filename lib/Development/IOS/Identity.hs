module Development.IOS.Identity
  ( Password
  , KeychainIdentity(..)
  , Identity(..)
  , identitiesMaybeEqual
  ) where

type Password = IO String

data KeychainIdentity = KeychainIdentity String
  deriving (Eq, Ord)

instance Show KeychainIdentity where
  show (KeychainIdentity name) = name

data Identity
  = IdentityKeychain KeychainIdentity
  | IdentityPKCS12File FilePath (Maybe Password)

instance Show Identity where
  show (IdentityKeychain keychainIdentity) = show keychainIdentity
  show (IdentityPKCS12File path _) = path

-- | Compares identities for equality.  May give false
-- negatives.
identitiesMaybeEqual :: Identity -> Identity -> Bool
identitiesMaybeEqual (IdentityKeychain a) (IdentityKeychain b) = a == b
identitiesMaybeEqual (IdentityPKCS12File a _) (IdentityPKCS12File b _) = a == b
identitiesMaybeEqual _ _ = False
