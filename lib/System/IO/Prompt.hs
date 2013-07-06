module System.IO.Prompt
  ( prompt
  , prompt'
  , promptForever

  , promptFile
  , promptExistingFile
  , promptMenu
  ) where

import Control.Monad
import System.Console.Haskeline
import System.Directory

import Development.Spaceport.Util
import System.Environment.Expand

promptPrim :: String -> InputT IO (Maybe String)
promptPrim = getInputLine . addSpaceSuffix

prompt :: String -> IO (Maybe String)
prompt = runInputT settings . promptPrim
  where settings = setComplete noCompletion defaultSettings

prompt' :: String -> IO String
prompt' = maybe (fail "User aborted") return <=< prompt

promptForever :: IO (Either String a) -> IO (Maybe a)
promptForever f = f >>= \ mX -> case mX of
  Left "" -> return Nothing
  Left _ -> do
    putStrLn "Invalid input"
    promptForever f
  Right x -> return $ Just x

addSpaceSuffix :: String -> String
addSpaceSuffix s = case reverse s of
  (' ':_) -> s
  [] -> ""
  _ -> s ++ " "

promptFile :: String -> IO (Maybe FilePath)
promptFile message = do
  maybePath <- runInputT settings $ promptPrim message
  case maybePath of
    Nothing -> return Nothing
    Just path -> liftM Just $ expandPath path
  where
    settings = setComplete completeFilenameUnescaped defaultSettings
    completeFilenameUnescaped input = do
      (unusedPart, completions) <- completeWord Nothing "" listFiles input
      return (unusedPart, map normalizeCompletion completions)

    -- Set 'isFinished' to 'False' to prevent adding
    -- shell-style space/quote suffix to filename.
    normalizeCompletion completion = completion { isFinished = False }

promptExistingFile :: String -> IO (Either String FilePath)
promptExistingFile message = do
  maybePath <- promptFile message
  case maybePath of
    Nothing -> return $ Left ""
    Just path -> do
      exists <- doesFileExist path
      return $ if exists
        then Right path
        else Left path

imap :: (Int -> a -> b) -> [a] -> [b]
imap f = map (uncurry f) . zip [0..]

promptMenu :: [(String, a)] -> String -> IO (Either String a)
promptMenu options message = do
  sequence_ $ imap printOption options
  maybeString <- prompt $ addSpaceSuffix message
    ++ "[1-" ++ show (length options) ++ "]"
  case maybeString of
    Nothing -> return $ Left ""
    Just string -> return . maybe (Left string) Right $ do
      index <- maybeRead string
      (_, value) <- options `indexMaybe` (index - 1)
      return value

  where
    printOption index (str, _) = putStrLn
      $ "[" ++ show (index + 1) ++ "] " ++ str
