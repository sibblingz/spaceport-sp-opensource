module Development.Spaceport.Common.Tools
  ( Parameter
  , invoke'
  , option
  , maybeOption
  , options
  , flag
  , flagIf
  , arg
  , args
  , (~>)
  , (~>>)
  , needKey
  , needKeys
  ) where

import Control.Monad
import Development.Shake

import Development.Spaceport.Util

type Parameter = [String]

invoke' :: FilePath -> [Parameter] -> Action ()
invoke' = system' ..:. concat

option :: String -> String -> Parameter
option name value = [name, value]

maybeOption :: String -> Maybe String -> Parameter
maybeOption name (Just value) = [name, value]
maybeOption _name Nothing = []

options :: String -> [String] -> Parameter
options = concatMap . option

flag :: String -> Parameter
flag = return

flagIf :: Bool -> String -> Parameter
flagIf x y = [y | x]

arg :: String -> Parameter
arg = return

args :: [String] -> Parameter
args = id

(~>) :: (Show a) => String -> a -> Rules ()
key ~> value = addOracle [key] $ return [show value]

(~>>) :: (Show a) => String -> [a] -> Rules ()
key ~>> values = addOracle [key] $ return (map show values)

needKey :: String -> Action ()
needKey key = void $ askOracle [key]

needKeys :: [String] -> Action ()
needKeys = mapM_ needKey
