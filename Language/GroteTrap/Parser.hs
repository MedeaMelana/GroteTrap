-- | Generates a full parser from a language and offers some utility functions for immediate evaluation.
module Language.GroteTrap.Parser (

  -- * Parsing and reading
  parseSentence, readParseTree, readExpression
  
  ) where

import Language.GroteTrap.Lexer
import Language.GroteTrap.Language
import Language.GroteTrap.ParseTree
import Language.GroteTrap.Range
import Language.GroteTrap.Util

import Data.List (groupBy, sortBy)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Pos (newPos)
import qualified Text.ParserCombinators.Parsec.Expr as P

withEOF :: Show tok => GenParser tok st t -> GenParser tok st t
withEOF p = do v <- p; eof; return v

-- | Given a language and a string, yields the parse tree.
parseSentence :: Monad m => Language a -> String -> m ParseTree
parseSentence lang input = tokenize lang input >>=
                            run "tokens" (withEOF $ pTree lang) . filter (not . isWhite . snd)

-- | Given a language and a string, yields the parse tree or throws an exception.
readParseTree :: Language a -> String -> ParseTree
readParseTree lang = fromError . parseSentence lang

-- | Given a language and a string, parses and evaluates the string.
readExpression :: Language a -> String -> a
readExpression lang = evaluate lang . readParseTree lang

pTree :: Language a -> GenParser TokenPos () ParseTree
pTree lang = P.buildExpressionParser (buildOperatorTable $ operators lang) (pUnit lang)

pUnit :: Language a -> GenParser TokenPos () ParseTree
pUnit lang = choice [pCall lang, pId, pInt, pParens lang]

pId :: GenParser TokenPos () ParseTree
pId = tok f where
  f (pos, TId name) = Just $ PId pos name
  f _ = Nothing

pInt :: GenParser TokenPos () ParseTree
pInt = tok f where
  f (pos, TInt v) = Just $ PInt pos v
  f _ = Nothing

pCall :: Language a -> GenParser TokenPos () ParseTree
pCall lang = do
  (begin,name) <- tok f
  static TOpen
  args <- sepBy (pTree lang) (static TComma)
  (end,_) <- static TClose
  return $ PCall (begin,end) name args
  where f (pos, TFunction name) = Just (pos, name)
        f _ = Nothing

pParens :: Language a -> GenParser TokenPos () ParseTree
pParens lang = do
  (begin,_) <- static TOpen
  v <- pTree lang
  (end,_)   <- static TClose
  return $ PParens (begin, end + 1) v

buildOperatorTable :: [Operator a] -> P.OperatorTable TokenPos () ParseTree
buildOperatorTable = map (map buildOperator) . orderedOperators

buildOperator :: Operator a -> P.Operator TokenPos () ParseTree
buildOperator (Unary _ fix _ tok) = xFix fix (pUna tok)
buildOperator (Binary _ fix _ tok) = P.Infix (pBin tok) (infixX fix)
buildOperator (Assoc _ _ tok) = P.Infix (pList tok) P.AssocLeft

xFix :: Fixity1 -> GenParser t st (a -> a) -> P.Operator t st a
xFix Prefix  = P.Prefix
xFix Postfix = P.Postfix

infixX :: Fixity2 -> P.Assoc
infixX InfixL = P.AssocLeft
infixX InfixR = P.AssocRight

orderedOperators :: [Operator a] -> [[Operator a]]
orderedOperators = groupBy equalPriority . sortBy orderPriority where
  equalPriority a1 a2 = opPrio a1 == opPrio a2
  orderPriority a1 a2 = opPrio a1 `compare` opPrio a2

pList :: String -> GenParser TokenPos () (ParseTree -> ParseTree -> ParseTree)
pList token = do
  (pos, _) <- static $ TOperator token
  return $ assimilate token (pos, pos + length token)

assimilate :: String -> Range -> ParseTree -> ParseTree -> ParseTree
assimilate token range pt1@(PList rs tok ps) pt2
  | token == tok  = PList (rs ++ [range]) token (ps ++ [pt2])
  | otherwise     = PList [range] token [pt1,pt2]
assimilate token range pt1 pt2
  = PList [range] token [pt1,pt2]


pBin :: String -> GenParser TokenPos () (ParseTree -> ParseTree -> ParseTree)
pBin token = do
  (pos, _) <- static $ TOperator token
  return $ PBinary (pos, pos + length token) token

pUna :: String -> GenParser TokenPos () (ParseTree -> ParseTree)
pUna token = do
  (pos, _) <- static $ TOperator token
  return $ PUnary (pos, pos + length token) token

tok :: (TokenPos -> Maybe a) -> GenParser TokenPos st a
tok = token show (newPos "tokens" 1 . fst)

static :: Token -> GenParser TokenPos () TokenPos
static t = tok (\tp@(_,x) -> if x == t then Just tp else Nothing)
