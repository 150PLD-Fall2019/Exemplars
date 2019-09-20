module Arith where 

import Text.Parsec
import Control.Monad.Identity

data Exp = 
    Plus Exp Exp
  | Minus Exp Exp
  | Times Exp Exp
  | Div Exp Exp
  | Const Int
  deriving (Show,Eq)

type Parser a =
  ParsecT       -- ParsecT is a monad transformer i.e. a Monad parameterized by another
   String       -- the stream we will be consuming monadically is a string
   ()           -- we do not need to get or put any state to parse arithmetic
   Identity     -- let the monad we are parameterized by (evaluate to) be id
   a            -- To be a monad we still need kind :: * -> *



-- a combinator that tries the first parser, and if it failes, does not consume
-- any input and then tries the next parser
-- note, reliance on this combinator may make debugging ambiguity difficult
--
(<||>) :: Parser a -> Parser a -> Parser a
p1 <||> p2 = (try p1) <|> p2


-- In the future, we will be defining a tokenizer (a function from string to symbols) 
-- and a parser (a function from symbols to our AST) separately using help functions
-- parsec provides. Since those are not needed for this simple language, we will
-- be doing both at the same time here and not introducing a bunch of boilerplate.

-- In this exercise you will be parsing a prefix (scheme/lisp) notation for the
-- arithmetic language represented in the ADT above.
-- helpful docs: http://hackage.haskell.org/package/parsec3-1.0.1.8/docs/Text-Parsec-Char.html
-- My solution uses:
--   spaces :: Parser ()
--   string :: String -> Parser String
--   many1   :: Parser a -> Parser [a]
--   digit  :: Parser Char
--   read   :: (Read a) => String -> a
parsePrefix :: Parser Exp 
parsePrefix = undefined 
  
parseArith :: String -> Exp 
parseArith s = 
  case runParser parsePrefix () "" s of
    Left errMsg -> error (show errMsg)
    Right e     -> e
