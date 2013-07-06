{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}

module System.Java.Locate
  ( JVMNotFound(..)

  , findJVMPath
  , getJVMPath
  ) where

import Data.Typeable
import System.Directory

#ifdef WINDOWS_TOOLS
import Control.Applicative
import Control.Monad (guard)
import Data.Bits ((.|.))
import Data.List (nub)
import Data.Maybe (catMaybes)
import Development.Spaceport.Util (findM)
import System.FilePath
import System.IO.Error
#endif

import qualified Control.Exception as Ex

#ifdef WINDOWS_TOOLS
import qualified GHC.IO.Exception
import qualified System.Win32.Registry as Reg
import qualified System.Win32.Types as Reg (HKEY)
#endif

data JVMNotFound = JVMNotFound
  deriving (Typeable)

instance Show JVMNotFound where
  show JVMNotFound = "Java Runtime Environment not found"

instance Ex.Exception JVMNotFound

#ifdef WINDOWS_TOOLS

registryLocations :: [String]
registryLocations =
  [ "SOFTWARE\\JavaSoft\\Java Runtime Environment"
  , "SOFTWARE\\Wow6432Node\\JavaSoft\\Java Runtime Environment"
  ]

findJRERootKeysWithRegsam :: Reg.REGSAM -> IO [Reg.HKEY]
findJRERootKeysWithRegsam regsam
  = fmap catMaybes . flip mapM registryLocations $ \ loc -> ignoreKeyNotFound
  $ Just <$> Reg.regOpenKeyEx Reg.hKEY_LOCAL_MACHINE loc (regsam .|. Reg.kEY_READ)

-- We look in all the registries we can find: current, 32-bit, 64-bit.
findJRERootKeys :: IO [(Reg.HKEY, Reg.REGSAM)]
findJRERootKeys
  = concat <$> mapM findWithRegsam regsams
  where
    regsams = [0, kEY_WOW64_64KEY, kEY_WOW64_32KEY]
    kEY_WOW64_64KEY = 0x0100
    kEY_WOW64_32KEY = 0x0200
    findWithRegsam regsam = do
      keys <- findJRERootKeysWithRegsam regsam
      return $ map (\ key -> (key, regsam)) keys

findJREInstallation :: Reg.HKEY -> Reg.REGSAM -> IO (Maybe FilePath)
findJREInstallation root regsamAdd = do
  currentVersion <- Reg.regQueryValue root (Just "CurrentVersion")
  key <- Reg.regOpenKeyEx root currentVersion regsam
  fmap Just $ Reg.regQueryValue key (Just "JavaHome")
  where regsam = Reg.kEY_READ .|. regsamAdd

ignoreKeyNotFound :: (Alternative f) => IO (f a) -> IO (f a)
ignoreKeyNotFound = Ex.handleJust
  (guard . isKeyNotFound)
  (const $ return empty)
  where
    isKeyNotFound err
      = ioeGetErrorType err == GHC.IO.Exception.InvalidArgument
      || isDoesNotExistError err

findJREInstallations :: IO [FilePath]
findJREInstallations = fmap (nub . catMaybes)
  $ mapM (uncurry findJREInstallation) =<< findJRERootKeys

findJVMPath :: IO (Maybe FilePath)
findJVMPath = do
  installations <- findJREInstallations
  findM doesFileExist
    $ map (</> ("bin" </> "java.exe")) installations

#else
-- !WINDOWS_TOOLS

findJVMPath :: IO (Maybe FilePath)
findJVMPath = findExecutable "java"

#endif

getJVMPath :: IO FilePath
getJVMPath
  = maybe (Ex.throwIO JVMNotFound) return
  =<< findJVMPath
