-- | The Unparse type class and its 'ParseTree' instance, as well as some text manipulation.
module Language.GroteTrap.Unparse (

  -- * Class Unparse
  Unparse(..),
  
  -- * Text utility functions
  merge, over

  ) where

import Language.GroteTrap.Range
import Language.GroteTrap.ParseTree


------------------------------------
-- Class Unparse
------------------------------------


-- | Types that are unparsable. Unparsing is like prettyprinting, except that instead of pretty source the original source code is retrieved. This means unparsing is only possible for values that were the result of an earlier parse.
class Unparse p where
  unparse :: p -> String


------------------------------------
-- instance Unparse ParseTree
------------------------------------


instance Unparse ParseTree where
  unparse = foldParseTree $ ParseTreeAlg
    ( \pos name -> indent pos name )
    ( \pos value -> indent pos $ show value )
    unparseUnary
    unparseBinary
    unparseNary
    unparseCall
    unparseParens


indent :: Pos -> String -> String
indent n s = replicate n ' ' ++ s


unparseUnary :: Range -> String -> String -> String
unparseUnary (begin, _) op sub = indent begin op `over` sub


unparseBinary :: Range -> String -> String -> String -> String
unparseBinary (begin, _) op left right = left `over` indent begin op `over` right


unparseNary :: [Range] -> String -> [String] -> String
unparseNary ranges op children = foldl over "" children `over` foldl over "" (map place ranges)
  where place (begin, _) = indent begin op


unparseParens :: Range -> String -> String
unparseParens (begin, end) sub = indent begin "(" `over` sub `over` indent (end - 1) ")"


unparseCall = error "Whoops! Not implemented"


------------------------------------
-- Text utility functions
------------------------------------


-- | @over upper lower@ places @upper@ over @lower@. The resulting string has the same characters as @upper@ does, except where @upper@ contains spaces; at those positions, the character from @lower@ shows. If @lower@ is longer than @upper@, @upper@ is padded with enough spaces to show all rest of @lower@.
over :: String -> String -> String
over upper lower = take n $ zipWith f (pad upper) (pad lower)
  where f ' ' b = b
        f  a  _ = a
        pad = (++ repeat ' ')
        n = length upper `max` length lower

-- | Merge folds many strings 'over' each other.
merge :: [String] -> String
merge = foldr over ""
