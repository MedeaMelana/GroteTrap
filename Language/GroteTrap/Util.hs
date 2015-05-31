-- | Utility functions.
--
-- This module re-exports module Control.Monad.Error so that 'fromError' can be used as @Monad m => m a -> a@.
module Language.GroteTrap.Util (fromError, run, module Control.Monad.Error) where

import Text.ParserCombinators.Parsec
import Control.Monad.Error ()

-- | Either returns the value or throws an exception.
fromError :: Either String a -> a
fromError = either error id

-- | @run sourceName p input@ runs the specified parser on the input, returning the result in a monad.
run :: Monad m => String -> GenParser tok () a -> [tok] -> m a
run name p input = case runParser p () name input of
  Left err  -> fail ("parse error at " ++ show err)
  Right v   -> return v
