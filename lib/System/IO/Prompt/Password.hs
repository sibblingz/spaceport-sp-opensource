module System.IO.Prompt.Password
  ( promptPassword
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import System.Console.Haskeline
import System.IO

-- FIXME Factor to use promptForever or something.
promptPassword
  :: String                            -- ^ Prompt message.
  -> (String -> IO (Either String a))  -- ^ Verify password.
  -> IO (Maybe a)
promptPassword message verify = runMaybeT loop
  where
    loop = do
      maybePass <- liftIO . runInputT defaultSettings
        $ getPassword (Just '*') message
      pass <- liftMaybe maybePass
      verified <- liftIO $ verify pass
      case verified of
        Right result -> return result
        Left err -> do
          guard $ not (null pass)  -- Empty means exit.
          liftIO $ hPutStrLn stderr err
          loop
    liftMaybe = MaybeT . return
