-- | The Language type that is the core of GroteTrap.
module Language.GroteTrap.Language (

  -- * Language
  Language(..), language,
  
  -- * Operators
  Operator(..),
  Fixity1(..), Fixity2(..),
  isUnary, isBinary, isAssoc,
  findOperator,

  -- * Functions
  Function(..),
  findFunction,
  function1, function2

  ) where


------------------------------------
-- Language
------------------------------------


-- | Language connects the syntax of identifiers, numbers, operators and functions with their semantics. GroteTrap is able to derive a parser and evaluator from a Language, as well as convert between source text selections and tree selections.
data Language a  =   Language
  { variable     ::  Maybe (String -> a)
  , number       ::  Maybe (Int -> a)
  , operators    ::  [Operator a]
  , functions    ::  [Function a]
  }

-- | An empty language. Use this as the starting base of your languages, setting only those fields that are of importance.
language :: Language a
language = Language
  { variable    = Nothing
  , number      = Nothing
  , operators   = []
  , functions   = []
  }

------------------------------------
-- Operators
------------------------------------


-- | Representation of an operator.
data Operator a
  = -- | An operator expecting one operand.
    Unary
    { opSem1        :: a -> a
    , opFixity1     :: Fixity1
    , opPrio        :: Int
    , opToken       :: String
    }
  | -- | A non-associative operator expecting two operands.
    Binary
    { opSem2        :: a -> a -> a
    , opFixity2     :: Fixity2
    , opPrio        :: Int
    , opToken       :: String
    }
  | -- | An infix associative operator that chains together many operands.
    Assoc
    { opSemN        :: [a] -> a
    , opPrio        :: Int
    , opToken       :: String
    }


-- | Fixity for unary operators.
data Fixity1
  = Prefix  -- ^ The operator is written before its operand.
  | Postfix -- ^ The operator is written after its operand.
  deriving (Show, Enum, Eq)


-- | Fixity for infix binary operators.
data Fixity2
  = InfixL  -- ^ The operator associates to the left.
  | InfixR  -- ^ The operator associates to the right.
  deriving (Show, Enum, Eq)


isUnary, isBinary, isAssoc  ::  Operator a -> Bool
isUnary   (Unary  _ _ _ _)  =   True
isUnary   _                 =   False
isBinary  (Binary _ _ _ _)  =   True
isBinary  _                 =   False
isAssoc   (Assoc _ _ _)     =   True
isAssoc   _                 =   False


-- | @findOperator name p os@ yields the operator from @os@ that matches the predicate @p@ and has token @name@. Fails if there are no or several matching operators.
findOperator :: Monad m => String -> (Operator a -> Bool) -> [Operator a] -> m (Operator a)
findOperator name f os = case filter (\o -> f o && opToken o == name) os of
  []  -> fail ("no such operator: " ++ name)
  [o] -> return o
  _   -> fail ("duplicate operator: " ++ name)


------------------------------------
-- Functions
------------------------------------


-- | Representation of a function.
data Function a = Function
  { fnSem   :: [a] -> a
  , fnName  :: String
  }


-- | Lifts a unary function to a 'Function'.
function1 :: (a -> a) -> String -> Function a
function1 f = Function (\[x] -> f x)


-- | Lifts a binary function to a 'Function'.
function2 :: (a -> a -> a) -> String -> Function a
function2 f = Function (\[x, y] -> f x y)


-- | Yelds the function with the specified name. Fails if there are no or several matching functions.
findFunction :: Monad m => String -> [Function a] -> m (Function a)
findFunction name fs = case filter ((== name) . fnName) fs of
  []  -> fail ("no such function: " ++ name)
  [f] -> return f
  _   -> fail ("duplicate function: " ++ name)
