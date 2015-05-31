-- | Deriving a lexer from a 'Language'.
module Language.GroteTrap.Lexer (
  -- * Types
  Token(..),
  TokenPos,
  
  -- * Tokenizing
  run, tokenize, isWhite
  
  ) where

import Language.GroteTrap.Language
import Language.GroteTrap.Range
import Language.GroteTrap.Util

import Text.ParserCombinators.Parsec

-- | The tokenizer produces a list of tokens.
data Token
  = TId String
  | TInt Int
  | TOperator String
  | TFunction String
  | TOpen
  | TClose
  | TComma
  | TWhite Int
  deriving (Eq, Show)

-- | Whether the token is whitespace.
isWhite :: Token -> Bool
isWhite (TWhite _) = True
isWhite _ = False

type TokenPos = (Pos, Token)

-- | When given a language, transforms a list of characters into a list of tokens.
tokenize :: Monad m => Language a -> String -> m [TokenPos]
tokenize lang = run "characters" (many $ pToken lang)

pToken :: Language a -> Parser TokenPos
pToken lang = choice [try $ pFunction $ functions lang, pId, pInt, pOperator $ operators lang, pOpen, pClose, pComma, pWhite]

savePos :: GenParser tok st t -> GenParser tok st (Pos, t)
savePos p = do pos <- getPos; v <- p; return (pos, v)

getPos = do
  pos <- getPosition
  return $ sourceColumn pos - 1 --TODO: lines

pId :: Parser TokenPos
pId = savePos $ do
  c  <- letter
  cs <- many $ choice [letter, digit, char '_']
  return $ TId (c:cs)

pInt :: Parser TokenPos
pInt = savePos $ many1 digit >>= (return . TInt . read)

pOperator :: [Operator a] -> Parser TokenPos
pOperator = choice . map pOneOperator

pOneOperator :: Operator a -> Parser TokenPos
pOneOperator op = savePos $ string (opToken op) >> return (TOperator (opToken op))

pFunction :: [Function a] -> Parser TokenPos
pFunction = choice . map pOneFunction

pOneFunction :: Function a -> Parser TokenPos
pOneFunction fun = savePos $ string (fnName fun) >> return (TFunction (fnName fun))

pOpen :: Parser TokenPos
pOpen = savePos $ char '(' >> return TOpen

pClose :: Parser TokenPos
pClose = savePos $ char ')' >> return TClose

pComma :: Parser TokenPos
pComma = savePos $ char ',' >> return TComma

pWhite :: Parser TokenPos
pWhite = savePos $ do spaces <- many1 space; return $ TWhite $ length spaces
