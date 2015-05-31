module Language.GroteTrap.Range (

  -- * Types
  Pos, Range, Ranged(..),
  
  -- * Utility functions
  distRange, inRange, includes, unionRange, size, validRange

  ) where

-- | A @Pos@ is a position in between two elements in a list. For example, position @0@ marks the beginning of the list, and position @length list@ marks the end of the list. There are @n + 1@ valid positions for a list of length @n@.
type Pos = Int

--    1   :: Pos
--    |
-- 0 1 2 3 4 5 6 7 8 9
--  k a a s b r o o d
-- 0 1 2 3 4 5 6 7 8 9
-- \_______/
--  (0,4) :: Range
-- | A range's positions mark the begin and end of a sublist, respectively.
type Range = (Pos, Pos)

-- | Something that knows its range as sublist in a larger list. Minimal complete definition: either 'range' or both 'begin' and 'end'.
class Ranged a where
  -- | Yields the element's range.
  range :: a -> Range
  range x = (begin x, end x)

  -- | Yields the element's begin position.
  begin :: a -> Pos
  begin = fst . range

  -- | Yields the element's end position.
  end :: a -> Pos
  end = snd . range

-- | A range's size is the number of elements it contains.
size :: Range -> Int
size (b,e) = e-b

-- | Whether a position falls within a range, including the range's edges.
inRange :: Pos -> Range -> Bool
inRange pos (begin, end) = pos >= begin && pos <= end

-- | @unionRange x y@ yields the smallest range z such that @x ``includes`` z@ and @y ``includes`` z@.
unionRange :: Range -> Range -> Range
unionRange = min **** max

-- | Yields whether the second argument completely falls within the first argument.
includes :: Range -> Range -> Bool
includes r (b,e) = b `inRange` r && e `inRange` r

-- | @distRange (b1, e1) (b2, e2)@ is defined as @|b1 - b2| + |e1 - e2|@.
distRange :: Range -> Range -> Int
distRange (b1,e1) (b2,e2) = db + de
  where db = abs (b1 - b2)
        de = abs (e1 - e2)

(****) :: (a -> b -> c) -> (d -> e -> f) -> (a,d) -> (b,e) -> (c,f)
(****) fl fr (a,d) (b,e) = (fl a b, fr d e)

-- | A range is valid if its positions are nonnegative and begin < end.
validRange :: Range -> Bool
validRange (b, e) = b >= 0 && e >= 0 && b <= e
