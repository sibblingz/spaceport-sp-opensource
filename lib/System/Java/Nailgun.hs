module System.Java.Nailgun
  ( NailgunServer(..)

  , JVMOption
  , ClassPath
  , JavaClass

  , JavaPath
  , NailgunServerPath
  , NailgunClientPath
  , Environment

  , runServer
  , runServer_

  , runClient
  ) where

import Control.Applicative
import Data.Word
import Network
import System.Java
import System.IO hiding (stdin, stdout, stderr)
import System.Process

import Development.Spaceport.Util

data NailgunServer
  = NailgunServer (Maybe HostName) PortNumber
  deriving (Show)

type NailgunServerPath = String
type NailgunClientPath = String

runServer
  :: JavaPath
  -> NailgunServerPath
  -> Maybe HostName
  -> Maybe PortNumber 
  -> [JVMOption]
  -> [ClassPath]
  -> Maybe Environment
  -> IO (NailgunServer, Handle, Handle, ProcessHandle)
runServer javaPath ngServerPath mHost mPort jvmOptions classPaths mEnv = do
  (stdin, stdout, stderr, pid) <- runJVM
    javaPath jvmOptions (ngServerPath : classPaths) Nothing mEnv
    "com.martiansoftware.nailgun.NGServer" serverArgs
  hClose stdin
  statusLine <- hGetLine stdout
  case parseStatusLine statusLine of
    Nothing -> fail "Failed to read Nailgun status line"
    Just s -> return (s, stdout, stderr, pid)
  where
    serverArgs = [hostPort]
    hostPort
      = maybe "" (++ ":") mHost
      ++ maybe "0" show mPort

runServer_
  :: JavaPath
  -> NailgunServerPath
  -> Maybe HostName
  -> Maybe PortNumber 
  -> [JVMOption]
  -> [ClassPath]
  -> Maybe Environment
  -> IO NailgunServer
runServer_ javaPath ngServerPath mHost mPort jvmOptions classPaths mEnv
  = (\ (s, _out, _err, _pid) -> s)
  <$> runServer javaPath ngServerPath mHost mPort jvmOptions classPaths mEnv

runClient
  :: NailgunClientPath
  -> NailgunServer
  -> JavaClass  -- ^ Entry point class.
  -> [String]   -- ^ Arguments.
  -> IO (Handle, Handle, Handle, ProcessHandle)
runClient ngClientPath (NailgunServer mHost port) javaClass args = do
  (Just stdin, Just stdout, Just stderr, pid) <- createProcess process
  return (stdin, stdout, stderr, pid)
  where
    process = (proc ngClientPath ngArgs)
      { std_in = CreatePipe
      , std_out = CreatePipe
      , std_err = CreatePipe
      }
    ngArgs = hostArgs ++ portArgs ++ [javaClass] ++ args
    hostArgs = maybe [] (\ h -> ["--nailgun-server", h]) mHost
    portArgs = ["--nailgun-port", show port]

initSafe :: [a] -> [a]
initSafe [] = []
initSafe xs = init xs

-- | See [note Nailgun status line].
parseStatusLine :: String -> Maybe NailgunServer
parseStatusLine statusLine = case reverse $ words statusLine of
  (portDot:"port":"interfaces,":"all":"on":"started":_)
    -> NailgunServer Nothing <$> parsePortDot portDot
  (portDot:"port":hostNameComma:"on":"started":_)
    -> NailgunServer (Just $ initSafe hostNameComma)
      <$> parsePortDot portDot
  _ -> Nothing
  where
    parsePortDot :: String -> Maybe PortNumber
    parsePortDot s = fmap fromIntegral
      (maybeRead (initSafe s) :: Maybe Word16)


-- [Note Nailgun status line]:
--
-- Java code printing the status line is [1]:
--
-- > System.out.println("NGServer "
-- >         + NGConstants.VERSION
-- >         + " started on "
-- >         + ((serverAddress == null)
-- >         ? "all interfaces"
-- >         : serverAddress.getHostAddress())
-- >         + ", port "
-- >         + runningPort
-- >         + ".");
--
-- [1]: https://github.com/martylamb/nailgun/blob/250578ff5bae9e3685212d02f3da5ad5b067607d/nailgun-server/src/main/java/com/martiansoftware/nailgun/NGServer.java#L508-516
