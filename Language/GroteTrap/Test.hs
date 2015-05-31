module Language.GroteTrap.Test where

import Language.GroteTrap.ShowTree
import Language.GroteTrap.Examples.Logic
import Language.GroteTrap.Selection
import Language.GroteTrap.Parser
import Language.GroteTrap.Unparse
import Language.GroteTrap.ParseTree (evaluate)
import Language.GroteTrap.Util ()
import Language.GroteTrap.Range
import Language.GroteTrap.Lexer (resultOf)

import Data.Maybe (fromJust)

ah = fromRight $ parseExpression logicLanguage "jan && alleman -> supermarkt"

raar = fromRight $ parseExpression logicLanguage "A && B -> (C || D || (C -> D && E) -> !D) && !C"

mooi = putStrLn $ showTree ah

finds = map (\x -> posToPath ah x) [0..length (unparse ah)]

suggestions p r@(b,e) = putStr $ unlines $ niceR : suggs
  where suggs = map (unparse . flip select p . fromJust . flip matchRange p) $ suggest p r
        niceR = replicate b ' ' ++ 
                  if size r == 0 then "/"
                                 else replicate (e-b) '_'

fromRight (Right r) = r
fromLeft (Left l) = l
