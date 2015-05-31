module ArithmeticExample where

import Language.GroteTrap

arith :: Language Int
arith = language
  { number    = id
  , operators =
      [ Assoc   sum             2 "+"
      , Binary  (-)     InfixL  2 "-"
      , Assoc   product         1 "*"
      , Binary  div     InfixL  1 "/"
      , Binary  (^)     InfixL  0 "^"
      ]
  , functions = [ function1 abs "abs" ]
  }

evalArith :: String -> Int
evalArith = readExpression arith

tree  = readParseTree arith "2 + 3 * (4 + 5) * 6"
demo0 = printTree tree
demo1 = printTree $ fromError $ follow tree [1,0]
demo2 = printTree $ fromError $ follow tree [1,1]
demo3 = range $ fromError $ follow tree [1,1]
demo4 = rangeToSelection tree (4,15) :: IO TreeSelection
demo5 = rangeToSelection tree (4,14) :: IO TreeSelection
demo6 = repair tree (4,14)
