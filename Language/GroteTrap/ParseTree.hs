-- | A generic parse tree. A 'ParseTree' and a 'Language' together provide enough information to fully evaluate the input sentence.
module Language.GroteTrap.ParseTree (

  -- * Type ParseTree
  ParseTree(..),
  
  -- * Catamorphisms
  ParseTreeAlg(..), foldParseTree,
  
  -- * Evaluation
  evaluate, evalRange
  
  ) where

import Language.GroteTrap.Language
import Language.GroteTrap.Range
import Language.GroteTrap.Trees

import Data.Maybe (fromJust)


-- | A generic parse tree.
data ParseTree
  = PId            Pos     String
  | PInt           Pos     Int
  | PUnary         Range   String   ParseTree
  | PBinary        Range   String   ParseTree   ParseTree
  | PList         [Range]  String  [ParseTree]
  | PCall          Range   String  [ParseTree]
  | PParens        Range            ParseTree
  deriving Show

-- | An algebra for parse trees catamorphisms.
data ParseTreeAlg a = ParseTreeAlg
  { algId       :: Pos   -> String  -> a
  , algInt      :: Pos   -> Int     -> a
  , algUnary    :: Range -> String  -> a -> a
  , algBinary   :: Range -> String  -> a -> a -> a
  , algList     :: [Range] -> String -> [a]  -> a
  , algCall     :: Range -> String  -> [a] -> a
  , algParens   :: Range -> a -> a
  }

-- | Folds parse trees using an algebra.
foldParseTree :: ParseTreeAlg a -> ParseTree -> a
foldParseTree (ParseTreeAlg f1 f2 f3 f4 f5 f6 f7) = f where
  f (PId a1 a2)           = f1 a1 a2
  f (PInt a1 a2)          = f2 a1 a2
  f (PUnary a1 a2 a3)     = f3 a1 a2 (f a3)
  f (PBinary a1 a2 a3 a4) = f4 a1 a2 (f a3) (f a4)
  f (PList a1 a2 a3)      = f5 a1 a2 (map f a3)
  f (PCall a1 a2 a3)      = f6 a1 a2 (map f a3)
  f (PParens a1 a2)       = f7 a1 (f a2)

instance Ranged ParseTree where
  range = foldParseTree (ParseTreeAlg var int una bin list call const) where
    var pos name = (pos, pos + length name)
    int pos v = (pos, pos + (length $ show v))
    una r _ c = r `unionRange` c
    bin _ _ (begin, _) (_, end) = (begin, end)
    list _ _ cs = (fst $ head cs, snd $ last cs)
    call (begin,_) _ ps = (begin, snd $ last ps)

instance Tree ParseTree where
  children p = case p of
    PUnary  _ _ c   -> [c]
    PBinary _ _ l r -> [l, r]
    PList   _ _ cs  -> cs
    PCall   _ _ cs  -> cs
    PParens _ c     -> [c]
    _               -> []

instance Selectable ParseTree where
  allowSubranges p = case p of
    PList _ _ _ -> True
    _           -> False

-- | Evaluates a parse tree from a language.
evaluate :: Language a -> ParseTree -> a
evaluate lang = foldParseTree (ParseTreeAlg eid eint euna ebin elst ecll epar) where
  eid  _            = fromJust $ variable lang
  eint _            = fromJust $ number   lang
  euna _ op         = opSem1  $ fromJust $ findOperator op isUnary  $ operators lang
  ebin _ op         = opSem2  $ fromJust $ findOperator op isBinary $ operators lang
  elst _ op         = opSemN  $ fromJust $ findOperator op isAssoc  $ operators lang
  ecll _ fun args   = fnSem     (fromJust (findFunction fun (functions lang))) args
  epar _            = id

-- | Evaluates part of a parse tree. The relevant part is indicated by the range.
evalRange :: Monad m => Language a -> ParseTree -> Range -> m [a]
evalRange lang tree range = do
  tsel <- rangeToSelection tree range
  expr <- select tree tsel
  return $ map (evaluate lang) expr
