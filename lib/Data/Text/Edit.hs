module Data.Text.Edit
  ( Offset
  , Change
  , mkChange
  , applyChanges
  ) where

import Data.Text (Text)

import qualified Data.Text as Text
import qualified Data.Vector as V

type Offset = Int

-- | Replace text between the two offsets with the given
-- text.
data Change = Change Offset Offset Text

-- | Constructor for 'Change'.
mkChange :: Offset -> Offset -> Text -> Change
mkChange start end text
  | start < 0 = err "start offset out of range (before 0)"
  | start > end = err "end offset out of range (before start offset)"
  | otherwise = Change start end text
  where err msg = error $ "Data.Text.Edit.change: " ++ msg

applyChanges :: [Change] -> Text -> Text
applyChanges changes text
  = V.foldl Text.append Text.empty chunks'
  where
    chunks :: V.Vector Text
    chunks = V.fromList
      $ map Text.singleton (Text.unpack text) ++ [Text.empty]

    chunks' = foldr applyChange chunks changes

    applyChange :: Change -> V.Vector Text -> V.Vector Text
    applyChange (Change start end contents) cs
      | not (inRange start) = err "start offset out of range"
      | not (inRange end) = err "end offset out of range"
      | otherwise = cs V.// (replace : deletions)
      where
        deletions = map (\ o -> (o, Text.empty)) [succ start .. pred end]
        replace = (start, contents)

        inRange o = 0 <= o && o < V.length cs
        err msg = error
          $ "Data.Text.Edit.applyChanges.applyChange: "
          ++ msg
