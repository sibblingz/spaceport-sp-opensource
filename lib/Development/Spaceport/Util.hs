module Development.Spaceport.Util
  ( concat2
  , mapM2
  , for
  , forM2
  , concatMapM
  , concatForM
  , findM

  , maybeRead

  , shouldIgnoreFile

  , maybeToEither
  , eitherToMaybe

  , (..:.)
  , ($$)

  , removeDirectoryRecursiveIfExists
  , removeFileIfExists

  , fullSet

  , readHumanBool
  , showHumanBool

  , unMaybe
  , unsafeUnMaybe
  , justIf
  , (?:)

  , errorToEither

  , unEither
  , unEitherString
  , mapLeft
  , fromRight

  , compose
  , catchIf
  , catchWhen

  , caselessEq
  , caselessElem

  , setConcat

  , readPasswordFromFile

  , falseM

  , bytestringToStringUTF8

  , oxfordList
  , orList
  , andList

  , ifM

  , indexMaybe

  , isLeft
  , isRight

  , ioLines
  , headDef
  , headIOLine
  ) where

import Control.Arrow
import Control.Monad
import Data.List
import Data.Maybe
import Data.Set (Set)
import Data.Text (Text)
import System.Directory
import System.FilePath
import System.IO.Error

import qualified Control.Exception as Ex
import qualified Data.ByteString as BS
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified System.IO.Unsafe

concat2 :: [([a], [b])] -> ([a], [b])
concat2 = (concat *** concat) . unzip

mapM2 :: (Monad m) => (a -> m ([b], [c])) -> [a] -> m ([b], [c])
mapM2 f xs = liftM concat2 $ mapM f xs

for :: [a] -> (a -> b) -> [b]
for = flip map

forM2 :: (Monad m) => [a] -> (a -> m ([b], [c])) -> m ([b], [c])
forM2 = flip mapM2

concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f = liftM concat . mapM f

concatForM :: (Monad m) => [a] -> (a -> m [b]) -> m [b]
concatForM = flip concatMapM

findM :: (Monad m) => (a -> m Bool) -> [a] -> m (Maybe a)
findM f = go
  where
    go [] = return Nothing
    go (x:xs) = do
      ok <- f x
      if ok
	then return (Just x)
	else go xs

-- Taken from Network.CGI.Protocol (package 'cgi').
maybeRead :: (Read a) => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

-- | Ignore dot files and files within dot directories.
shouldIgnoreFile :: FilePath -> Bool
shouldIgnoreFile path
  | any ("." `isPrefixOf`) $ splitDirectories path = True
  | otherwise = False

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither err Nothing = Left err
maybeToEither _ (Just x) = Right x

eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe = either (const Nothing) Just

(..:.) :: (a -> b -> c) -> (d -> b) -> a -> d -> c
(f ..:. g) x y = f x (g y)

removeDirectoryRecursiveIfExists :: FilePath -> IO ()
removeDirectoryRecursiveIfExists path
  = catchIf isDoesNotExistError
    (removeDirectoryRecursive path)
    (return ())

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists path
  = catchIf isDoesNotExistError
    (removeFile path)
    (return ())

-- | Performs a monadic operation on @x@ if @Just x@.
($$) :: (Monad m) => (a -> m b) -> Maybe a -> m (Maybe b)
m $$ (Just x) = liftM Just (m x)
_ $$ Nothing = return Nothing
infixr 0 $$

fullSet :: (Ord a, Bounded a, Enum a) => Set a
fullSet = Set.fromDistinctAscList [minBound .. maxBound]

readHumanBool :: Text -> Maybe Bool
readHumanBool text
  | any isFirst "ty1" = Just True
  | any isFirst "fn0" = Just False
  | otherwise = Nothing
  where
    isFirst :: Char -> Bool
    isFirst c
      = Text.toCaseFold (Text.singleton c)
      `Text.isPrefixOf` Text.toCaseFold text

showHumanBool :: Bool -> Text
showHumanBool True = Text.pack "yes"
showHumanBool False = Text.pack "no"

unMaybe :: (Monad m) => String -> Maybe a -> m a
unMaybe _ (Just x) = return x
unMaybe message Nothing = fail message

unsafeUnMaybe :: String -> Maybe a -> a
unsafeUnMaybe _ (Just x) = x
unsafeUnMaybe message Nothing = error message

justIf :: Bool -> a -> Maybe a
justIf cond x = if cond then Just x else Nothing

-- | Prepends 'Just' an element or 'Nothing' to a list.
(?:) :: Maybe a -> [a] -> [a]
Just x ?: xs = x : xs
Nothing ?: xs = xs
infixr 5 ?:

-- | Evaluates a value to WHNF, returning Left if an
-- exception is thrown.
errorToEither :: a -> Either Ex.ErrorCall a
errorToEither x
  = System.IO.Unsafe.unsafePerformIO $ Ex.catch
    (return $! Right $! x)
    (return . Left)

unEither :: (Ex.Exception e) => Either e a -> IO a
unEither (Right x) = return x
unEither (Left ex) = Ex.throwIO ex

unEitherString :: (Monad m) => Either String a -> m a
unEitherString (Right x) = return x
unEitherString (Left err) = fail err

mapLeft :: (e -> e') -> Either e a -> Either e' a
mapLeft f (Left err) = Left $ f err
mapLeft _ (Right x) = Right x

fromRight :: Either e a -> a
fromRight (Left _) = error "Development.Spaceport.Util.fromRight: Left"
fromRight (Right x) = x

compose :: [a -> a] -> a -> a
compose = foldr (.) id

catchIf
  :: (Ex.Exception e)
  => (e -> Bool)  -- ^ Predicate.
  -> IO a         -- ^ Computation.
  -> IO a         -- ^ Exception handler.
  -> IO a
catchIf p m handler
  = catchWhen p m (const handler)

catchWhen
  :: (Ex.Exception e)
  => (e -> Bool)  -- ^ Predicate.
  -> IO a         -- ^ Computation.
  -> (e -> IO a)  -- ^ Exception handler.
  -> IO a
catchWhen p = Ex.catchJust
  $ \ e -> if p e then Just e else Nothing

caselessEq :: Text -> Text -> Bool
caselessEq a b = Text.toCaseFold a == Text.toCaseFold b

caselessElem :: Text -> [Text] -> Bool
caselessElem x = any (`caselessEq` x)

setConcat :: (Ord a) => Set (Set a) -> Set a
setConcat = Set.fromList . concatMap Set.toList . Set.toList

readPasswordFromFile :: FilePath -> IO String
readPasswordFromFile path = do
  contents <- readFile path
  case filter (not . null) $ ioLines contents of
    [password] -> return password
    [] -> fail "Password file contains no passwords."
    _ -> fail "Password file should contain only one password."

falseM :: (Monad m) => a -> m ()
falseM = const (return ())

bytestringToStringUTF8 :: BS.ByteString -> String
bytestringToStringUTF8
  = Text.unpack . Text.decodeUtf8With onDecodeError
  where onDecodeError _message _word = Just '\xFFFD'

-- | Adds commas and a conjunction between strings.
--
-- oxfordList "or" [] = ""
-- oxfordList "or" ["a"] = "a"
-- oxfordList "or" ["a", "b"] = "a or b"
-- oxfordList "or" ["a", "b", "c"] = "a, b, or c"
oxfordList :: String -> [String] -> String
oxfordList conjunction xs = case xs of
  [] -> ""
  [x] -> x
  [x, y] -> x ++ " " ++ conjunction ++ " " ++ y
  _ -> intercalate ", " (init xs) ++ ", " ++ conjunction ++ " " ++ last xs

-- | Adds commas and 'or' between strings.
orList :: [String] -> String
orList = oxfordList "or"

-- | Adds commas and 'and' between strings.
andList :: [String] -> String
andList = oxfordList "and"

ifM :: (Monad m) => m Bool -> m a -> m a -> m a
ifM m t f = m >>= \ b -> if b then t else f

indexMaybe :: [a] -> Int -> Maybe a
indexMaybe xs index
  | index < 0 = Nothing
  | otherwise = listToMaybe $ drop index xs

isLeft :: Either e a -> Bool
isLeft (Left _) = True
isLeft _ = False

isRight :: Either e a -> Bool
isRight (Right _) = True
isRight _ = False

-- | Like 'lines', but ignores CR.
--
-- Some Windows tools print CRLF to terminate lines.  Some
-- Windows tools (ADB in particular) print CRCRLF to
-- terminate lines.  Stripping CR handles these cases.
ioLines :: String -> [String]
ioLines = lines . filter (/= '\r')

headDef :: a -> [a] -> a
headDef def [] = def
headDef _ (x:_) = x

headIOLine :: String -> String
headIOLine = headDef "" . ioLines
