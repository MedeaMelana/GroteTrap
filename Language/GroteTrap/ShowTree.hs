module Language.GroteTrap.ShowTree (showTree, printTree) where

import Language.GroteTrap.Range
import Language.GroteTrap.Trees
import Language.GroteTrap.Unparse

import Data.List (intersperse)

-- | Unparses a selectable tree type to a pretty tree representation.
showTree :: (Ranged a, Tree a, Unparse a) => a -> String
showTree x = unlines' $ map (showTreeAtDepth x) [0..depth x - 1]

-- | Writes showTree's result to stdout.
printTree :: (Ranged a, Tree a, Unparse a) => a -> IO ()
printTree = putStrLn . showTree

showTreeAtDepth :: (Ranged a, Tree a, Unparse a) => a -> Int -> String
showTreeAtDepth p d = foldr clear' (merge $ map unparse x) (concatMap children x)
  where x = selectDepth d p

unlines' = concat . intersperse "\n"

clear' :: (Ranged a) => a -> String -> String
clear' c s = clear (range c) s

clear :: Range -> String -> String
clear (begin, end) abc = a ++ map (const ' ') b ++ c where
  (a, b, c) = splitAt3 begin end abc

splitAt3 :: Int -> Int -> [a] -> ([a], [a], [a])
splitAt3 x y abc = (a, b, c) where
  (a, bc) = splitAt x abc
  (b, c) = splitAt (y - x) bc
