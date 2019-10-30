{-# LANGUAGE TypeFamilies #-}
module IR where

import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)
import qualified Language.Haskell.Meta.Parse as LHM
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH (Name, Exp, Dec, Pat, Type, Q)  
import Data.Functor.Identity 
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Control.Monad.Fail
import Prelude hiding (fail)
import Control.Monad.IO.Class (liftIO)
import Debug.Trace (trace)
{-- IR is a language to help make Intermediate representations for compiler passes
 -  For example: We can desugar lets into lambdas
 -  data E1 = Lam1 String E1 | App1 E1 E1 | Var1 String | Let1 String E1 E1
 -  data E2 = Lam2 String E2 | App2 E2 E2 | Var2 String
 -
 -  reduce1to2 :: E1 -> E2
 -  reduce1to2 (Let1 x e body) = App2 (Lam2 x (reduce1to2 body)) (reduce1to2 e)  
 -  reduce1to2 (Lam1 x body) = Lam2 x (reduce1to2 body)
 -  ...
 -
 -  promote2to1 (Lam2 x body) = Lam1 x (promote2to1 body)
 -  ...
 -
 -  Of all this code, only one case is interesting: the first reduce1to2 case
 -    the rest is mechanically derived from the structure
 -    even E2 can be derived from this interesting case 
 --}

-- IR QuasiQuoter:  [ir| .... |] 
ir :: QuasiQuoter
ir = QuasiQuoter {
         quoteExp  = const $ error "quoteExp not defined for IR"
       , quotePat  = const $ error "quotePat not defined for IR"
       , quoteType = const $ error "quoteType not defined for IR"
       , quoteDec  = irCodeGen . irParse
}
-- IR Lexer 
--   We can make a lexer help us skip comments and other nice things for free

lexer = P.makeTokenParser haskellDef


-- IR Parser
-- typical usage 1: [ir| reduce E where (Pat -> Exp) | ... |]
-- typical usage 2: [ir| data E = ... reducing (Pat -> Exp) | ... |] 

---- IR AST

data Reduction = R Pat Exp
  deriving (Show)

data IRRef = IRRef Type [Reduction]
  deriving (Show)

data IRDec = IRDec Dec  [Reduction]
  deriving (Show)

data IRAST =
    RefAST IRRef
  | DecAST IRDec 
  deriving (Show)

type Parser a = 
  ParsecT 
    String
    ()
    Identity
    a 

---- A grammar:
--   IR      ::= 'reduce' TYPE 'where' PATLIST
--             |  DEC 'reducing' PATLIST
--   TYPE    ::= <identifier>
--   DEC     ::= <TH.Dec>
--   PATLIST ::= {(PAT '->' EXP), '|'}+ 
--   PAT     ::= <TH.Pat>
--   EXP     ::= <Th.Exp>

-- helpful functions:
--   LHM.parseExp       :: String -> Either String Exp
--   LHM.parseDecs      :: String -> Either String [Dec]
--   LHM.parsePat       :: String -> Either String Pat
--   LHM.parseType      :: String -> Either String Type
--   (P.whiteSpace lexer) :: Parser ()
--   (P.symbol lexer)     :: String -> Parser String
--   <|>                :: Parser a -> Parser a -> Parser a
--   try                :: Parser a -> Parser a
--   manyTill           :: Parser a -> Parser b -> Parser [a]
--   eof                :: Parser () 
--   anyChar           :: Parser Char

-- Parser helper functions
p1 <||> p2 = (try p1) <|> p2

-- Turning the LHM parser into a Parsec Parser, thanks for your input, Nate!
  -- although it needs an input string explicitly passed into it still
  -- challenge: rewrite these eat the string off of the input stream
  --            instead of an explicit parameter

failEither :: (MonadFail m) => Either String a -> m a
failEither (Left err) = fail err
failEither (Right a)  = return a

parseDecs :: String -> Parser [Dec]
parseDecs = failEither . LHM.parseDecs 

parseExp :: String -> Parser Exp
parseExp = failEither . LHM.parseExp 

parsePat :: String -> Parser Pat
parsePat = failEither . LHM.parsePat 

parseType :: String -> Parser Type
parseType =  failEither . LHM.parseType

-- Monadic PrintF-style debugging function
traceM :: (Monad m) => String -> m ()
traceM s = trace s (return ())

---- Parsec implementation of the parser 
irParser :: Parser IRAST
irParser = refParser <||> decParser 
  where
    ws  = P.whiteSpace lexer
    sym s = (P.symbol lexer) s >> return ()
    refParser = do
      ws
      sym "reduce"
      --traceM "Parser ate a reduce keyword"
      typeBlob <- manyTill anyChar $ try $  
                    do 
                       space 
                       (sym "where")
      --traceM "Parser ate a typeBlob and a where keyword"
      ty       <- parseType typeBlob
      --traceM "Parser converted typeBlob to Type"
      patList  <- parsePatList
      --traceM "Parser ate a patList"
      return $ RefAST $ IRRef ty patList    
    decParser = do
      ws
      decBlob <- manyTill anyChar $ try (sym "reducing")
      decs    <- parseDecs decBlob 
      dec <- 
        case decs of
          []  -> fail "reduce was given no valid declarations"
          [x] -> return x
          _   -> fail "reduce was given too many declarations"
      patList <- parsePatList
      return $ DecAST $ IRDec dec patList
    parsePatList = many1 parsePatExp
    parsePatExp  = do
      ws
      -- traceM "begin parsePatExp"
      patBlob <- manyTill anyChar $ try $
                   do
                     space
                     (sym "->")
                     --traceM "parsed the '->' after the pattern"
      expBlob <- manyTill anyChar $ 
                   do
                     ((ws >> sym "|") <||> eof)
                     --traceM "parsed the | or the eof after the expression"
      pat <- parsePat patBlob
      exp <- parseExp expBlob
      return $ R pat exp
      
      
irParse :: String -> IRAST
irParse decString  = 
  case runParser irParser () "" decString of 
    Left err  -> error $ show err
    Right ast -> ast
  
-- IR Host Language (runtime)  Library

class IR e where  
  type Target e           -- http://dev.stephendiehl.com/hask/#type-families-1
  reduce   :: e -> Target e  -- a lossy rewrite from a high level IR to a low level one
  promote  :: Target e -> e    -- a loss-less rewrite from a low level IR to a high level one

-- IR Code Generator
irCodeGen :: IRAST -> Q [Dec]
irCodeGen = undefined


