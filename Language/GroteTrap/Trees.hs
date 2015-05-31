-- | A class for tree types and representations of selections on tree types, as well as functions for converting between text and tree selections.
module Language.GroteTrap.Trees (

  -- * Paths and navigation
  Path, root,
  Nav, up, into, down, left, right, sibling,
  
  -- * Tree types
  Tree(..), depth, selectDepth, flatten, follow, child,
  
  -- * Tree selections
  Selectable(..), TreeSelection,
  select, allSelections, selectionToRange, rangeToSelection, posToPath, isValidRange,
  
  -- * Suggesting and fixing
  suggestBy, suggest, repairBy, repair

  ) where

import Language.GroteTrap.Range
import Language.GroteTrap.Util

import Control.Monad (liftM)
import Data.List (sortBy, findIndex)
import Data.Maybe (isJust)
import Data.Ord (comparing)


------------------------------------
-- Paths and navigation
------------------------------------

-- | A path in a tree. Each integer denotes the selection of a child; these indices are 0-relative.
type Path  =  [Int]

-- | @root@ is the empty path.
root :: Path
root = []

-- | Navigation transforms one path to another.
type Nav = Path -> Path

-- | Move up to parent node. Moving up from root has no effect.
up :: Nav
up [] = []
up path = init path

-- | Move down into the nth child node. If @n@ is negative, the leftmost child is selected.
into    ::  Int -> Nav
into i = (++ [i `max` 0])

-- | Move down into first child node.
down :: Nav
down = into 0

-- | Move left one sibling.
left :: Nav
left = sibling (-1)

-- | Move right one sibling.
right :: Nav
right = sibling 1

-- | Move @n@ siblings to the right. @n@ can be negative. If the new child index becomes negative, the leftmost child is selected.
sibling ::  Int -> Nav
sibling 0 [] = []
sibling _ [] = error "the root has no siblings"
sibling d p  = into (last p + d) (up p)


------------------------------------
-- Parents and children
------------------------------------

-- | Tree types.
class Tree p where
  -- | Yields this tree's subtrees.
  children :: p -> [p]

-- | Pre-order depth-first traversal.
flatten :: Tree t => t -> [t]
flatten t = t : concatMap flatten (children t)

-- | Follows a path in a tree, returning the result in a monad.
follow :: (Monad m, Tree t) => t -> Path -> m t
follow parent [] = return parent
follow parent (t:ts) = do
  c <- child parent t
  follow c ts

-- | Moves down into a child.
child :: (Monad m, Tree t) => t -> Int -> m t
child t i
    | i >= 0 && i < length cs = return (cs !! i)
    | otherwise               = fail ("child " ++ show i ++ " does not exist")
  where cs = children t


-- | Yields the depth of the tree.
depth :: Tree t => t -> Int
depth t
  | null depths = 1
  | otherwise   = 1 + (maximum . map depth . children) t
  where depths = map depth $ children t


-- | Yields all ancestors at the specified depth.
selectDepth :: Tree t => Int -> t -> [t]
selectDepth 0 t = [t]
selectDepth d t = concatMap (selectDepth (d - 1)) (children t)



------------------------------------
-- Tree selections
------------------------------------


-- | Selection in a tree. The path indicates the left side of the selection; the int tells how many siblings to the right are included in the selection.
type TreeSelection = (Path, Int)


-- | Selectable trees.
class Tree t => Selectable t where
  -- | Tells whether complete subranges of children may be selected in this tree node. If not, valid TreeSelections in this tree always have a second element @0@.
  allowSubranges :: t -> Bool


-- | Enumerates all possible selections of a tree.
allSelections :: Selectable a => a -> [TreeSelection]
allSelections p = (root, 0) : subranges ++ recurse where
  subranges
    | allowSubranges p =
        [ ([from], to - from)
        | from <- [0 .. length cs - 2]
        , to <- [from + 1 .. length cs - 1]
        , from > 0 || to < length cs - 1
        ]
    | otherwise = []
  cs = children p
  recurse = concat $ zipWith label cs [0 ..]
  label c i = map (rt i) (allSelections c)
  rt i (path, offset) = (i : path, offset)

-- | Selects part of a tree.
select :: (Monad m, Tree t) => t -> TreeSelection -> m [t]
select t (path, offset) = (sequence . map (follow t) . take (offset + 1) . iterate right) path

-- | Computes the range of a valid selection.
selectionToRange :: (Monad m, Tree a, Ranged a) => a -> TreeSelection -> m Range
selectionToRange parent (path, offset) = do
  from <- follow parent path
  to   <- follow parent (sibling offset path)
  return (begin from, end to)


-- | Converts a specified range to a corresponding selection and returns it in a monad.
rangeToSelection :: (Monad m, Selectable a, Ranged a) => a -> Range -> m TreeSelection
rangeToSelection p (b, e)
  -- If the range matches that of the root, we're done.
  | range p == (b, e) =
      return (root, 0)

  | otherwise =
      -- Find the children whose ranges contain b and e.
      let cs     = children p
          ri pos = findIndex (inRange pos . range) cs
       in case (ri b, ri e) of

               (Just l, Just r) ->
                   if l == r
                   -- b and e are contained by the same child!
                   -- Recurse into child and prepend child index.
                   then liftM (\(path, offset) -> (l : path, offset)) $
                          rangeToSelection (cs !! l) (b, e)

                   else if allowSubranges p && begin (cs !! l) == b && end (cs !! r) == e
                   -- b is the beginning of l, and e is the end
                   -- of r: a selection of a range of children.
                   -- Note that r - l > 0; else it would've been
                   -- caught by the previous test.
                   -- This also means that there are many ways
                   -- to select a single node: either select it
                   -- directly, or select all its children.
                   then return ([l], r - l)

                   -- All other cases are bad.
                   else fail "text selection does not have corresponding tree selection"

               -- Either position is not contained
               -- within any child. Can't be valid.
               _ -> fail "text selection does not have corresponding tree selection"


-- | Returns the path to the deepest descendant whose range contains the specified position.
posToPath :: (Monad m, Tree a, Ranged a) => a -> Pos -> m Path
posToPath p pos = case break (inRange pos . range) (children p) of
  (_, [])   ->  if pos `inRange` range p
                  then return root
                  else fail ("tree does not contain position " ++ show pos)
  (no, c:_) ->  liftM (length no :) (posToPath c pos)


-- | Tells whether the text selection corresponds to a tree selection.
isValidRange :: (Ranged a, Selectable a) => a -> Range -> Bool
isValidRange p = isJust . rangeToSelection p


------------------------------------
-- Suggesting and fixing
------------------------------------


-- | Yields all possible selections, ordered by distance to the specified range, closest first.
suggestBy :: (Selectable a, Ranged a) => (Range -> Range -> Int) -> a -> Range -> [TreeSelection]
suggestBy cost p r = sortBy (comparing distance) (allSelections p) where
  distance = cost r . fromError . selectionToRange p

-- | @suggest@ uses 'distRange' as cost function.
suggest :: (Selectable a, Ranged a) => a -> Range -> [TreeSelection]
suggest = suggestBy distRange

-- | Takes @suggestBy@'s first suggestion and yields its range.
repairBy :: (Ranged a, Selectable a) => (Range -> Range -> Int) -> a -> Range -> Range
repairBy cost p = fromError . selectionToRange p . head . suggestBy cost p

-- | @repair@ uses 'distRange' as cost function.
repair :: (Ranged a, Selectable a) => a -> Range -> Range
repair = repairBy distRange
