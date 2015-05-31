module Language.GroteTrap.Show (lshow, format) where

import Data.Maybe
import Data.List
import Data.Generics hiding (Prefix)
import Language.GroteTrap.Language
import Language.GroteTrap.Parser

lshow lang = lshow' lang maxBound

lshow' :: Data a => Language a -> Int -> a -> String
lshow' lang contextPrio val =
  if isJust (variable lang) && con == toConstr (fromJust (variable lang) undefined)
  then fromJust $ gfindtype val
  else if isJust (number lang) && con == toConstr (fromJust (number lang) undefined)
  then show $ (fromJust $ gfindtype val :: Int)
  else fromJust $ lookup con [ (toConstr $ opCon op, lshow'Op lang contextPrio op val) | op <- operators lang ]
  where
    con = toConstr val

opCon :: Operator a -> a
opCon (Unary { opSem1 = o }) = o undefined
opCon (Binary { opSem2 = o }) = o undefined undefined
opCon (Assoc { opSemN = o }) = o undefined

gchildren :: (Data a, Typeable b) => a -> [b]
gchildren v = catMaybes $ gmapQ (Nothing `mkQ` Just) v

lshow'Op :: Data a => Language a -> Int -> Operator a -> a -> String
lshow'Op lang contextPrio op val = par $ case (op, gchildren val) of
    (Unary _ Prefix prio tok, [c]) ->
      tok ++ sh prio c
    (Unary _ Postfix prio tok, [c]) ->
      sh prio c ++ tok
    (Binary _ _ prio tok, [lhs, rhs]) ->
      sh prio lhs ++ " " ++ tok ++ " " ++ sh prio rhs
    (Assoc _ prio tok, _) ->
      concat $ intersperse (" " ++ tok ++ " ") $ map (sh prio) (head (gchildren val))
    _ ->
      error "unexpected number of children"
  where
    sh = lshow' lang
    par s
      | opPrio op >= contextPrio  = "(" ++ s ++ ")"
      | otherwise                 = s

-- | Formats a sentence according to a language.
format :: Data a => Language a -> String -> String
format lang = lshow lang . readExpression lang
