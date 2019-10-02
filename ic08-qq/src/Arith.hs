{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Arith where 

import Text.Parsec
import Control.Monad.Identity
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Text.Parsec.Token
import Text.Parsec.Language (haskell)

type Parser a =
  ParsecT       -- ParsecT is a monad transformer i.e. a Monad parameterized by another
   String       -- the stream we will be consuming monadically is a string
   ()           -- we do not need to get or put any state to parse arithmetic
   Identity     -- let the monad we are parameterized by (evaluate to) be id
   a            -- To be a monad we still need kind :: * -> *



-- a combinator that tries the first parser, and if it fails, does not consume
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
parsePrefix :: Parser AExp 
parsePrefix = 
  let   parseParens = do
          spaces
          string "("
          exp <- parsePrefix
          string ")"
          spaces
          return exp
        parsePlus = do
          spaces
          string "+"
          e1 <- parsePrefix
          spaces
          e2 <- parsePrefix
          spaces
          return $ Plus e1 e2
        parseTimes = do
          spaces
          string "*"
          e1 <- parsePrefix
          spaces
          e2 <- parsePrefix
          spaces
          return $ Times e1 e2
        parseMinus = do
          spaces
          string "-"
          e1 <- parsePrefix
          spaces
          e2 <- parsePrefix
          spaces
          return $ Minus e1 e2
        parseDiv = do
          spaces
          string "/"
          e1 <- parsePrefix
          spaces
          e2 <- parsePrefix
          spaces
          return $ Div e1 e2
        parseConst = do
          spaces
          n <- read <$> many1 digit
          spaces
          return $ Const n
        parseIntVar = do
          spaces
          x <- (identifier haskell) 
          spaces
          return $ IntVar $ mkName x
  in    parseConst  <||>
        parseParens <||>
        parsePlus   <||>
        parseTimes  <||>
        parseMinus  <||>
        parseDiv    <||>
        parseIntVar <?>  "Arithmetic Parser"

data AExp = 
    Plus AExp AExp
  | Minus AExp AExp
  | Times AExp AExp
  | Div AExp AExp
  | Const Int
  | IntVar Name
  deriving (Show,Eq)

eval :: AExp -> AExp
eval (Plus e1 e2) = 
  case (eval e1, eval e2) of
    (Const i1, Const i2) -> Const (i1 + i2)
    (e1',e2')            -> Plus e1' e2'
eval (Minus e1 e2) = 
  case (eval e1, eval e2) of
    (Const i1, Const i2) -> Const (i1 - i2)
    (e1',e2')            -> Minus e1' e2'
eval (Times e1 e2) = 
  case (eval e1, eval e2) of
    (Const i1, Const i2) -> Const (i1 * i2)
    (e1',e2')            -> Times e1' e2'
eval (Div e1 e2) = 
  case (eval e1, eval e2) of
    (Const i1, Const i2) -> Const (i1 `div` i2)
    (e1',e2')            -> Div e1' e2'
eval e = e



genArith :: AExp -> Q Exp
genArith e =
  case eval e of
    Const i -> return $ LitE $ IntegerL (toInteger i)
    IntVar x -> return $ VarE $ x
    Plus e1 e2 ->  [| $(genArith e1) + $(genArith e2) |]
    Minus e1 e2 -> [| $(genArith e1) - $(genArith e2) |]
    Times e1 e2 -> [| $(genArith e1) * $(genArith e2) |]
    Div e1 e2   -> [| $(genArith e1) `div` $(genArith e2) |]

parseArith :: String -> AExp 
parseArith s = 
  case runParser parsePrefix () "" s of
    Left errMsg -> error (show errMsg)
    Right e     -> e

arith :: QuasiQuoter
arith =  QuasiQuoter { 
                quoteExp = genArith . parseArith
              , quotePat = undefined -- quotePat :: String -> Q Pat
              , quoteDec = undefined -- quoteDec :: String -> Q Dec
              , quoteType = undefined -- quoteType :: String -> Q Type
              }
