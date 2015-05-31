{-# OPTIONS -fglasgow-exts #-}

module LogicExample where

import Language.GroteTrap
import Data.Generics hiding (Prefix)

import Data.Set hiding (map)


-- Logic data structure.

data Logic
  = Var String
  | Or [Logic]
  | And [Logic]
  | Impl Logic Logic
  | Not Logic
  deriving (Show, Eq, Typeable, Data)

type LogicAlg a =
  ( String -> a
  , [a] -> a
  , [a] -> a
  , a -> a -> a
  , a -> a
  )

foldLogic :: LogicAlg a -> Logic -> a
foldLogic (f1, f2, f3, f4, f5) = f where
  f (Var  a1   ) = f1 a1
  f (Or   a1) = f2 (map f a1)
  f (And  a1) = f3 (map f a1)
  f (Impl a1 a2) = f4 (f a1) (f a2)
  f (Not  a1   ) = f5 (f a1)


-- Language definition.

logicLanguage :: Language Logic
logicLanguage = language
  { variable  = Just Var
  , operators =
      [ Unary    Not  Prefix  0 "!"
      , Assoc    And          1 "&&"
      , Assoc    Or           2 "||"
      , Binary   Impl InfixR  3 "->"
      ]
  }


-- Evaluation.

type Environment = Set String

evalLogic :: Environment -> Logic -> Bool
evalLogic env = foldLogic ((`member` env), or, and, (||) . not, not)

readLogic :: Environment -> String -> Bool
readLogic env = evalLogic env . readExpression logicLanguage

-- Examples

appie :: ParseTree
appie = readParseTree logicLanguage "piet && klaas && maartje -> supermarkt"

demo1 = appie
demo2 = unparse $ appie
demo9 = evaluate logicLanguage appie
demo3 = printTree $ appie
demo4 = fromError $ follow appie root
demo5 = fromError $ follow appie [0]
demo6 = range $ fromError $ follow appie [0]
demo7 = lshow logicLanguage (And [Var "p", Var "q"])
