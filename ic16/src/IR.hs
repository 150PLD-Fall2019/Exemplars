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

newtype Reduction = R (Pat,Exp)
  deriving (Show)

data IRRef = IRRef Name [Reduction]
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
--   (whiteSpace lexer) :: Parser ()
--   (symbol lexer)     :: Parser String
--   <|>                :: Parser a -> Parser a -> Parser a
--   try                :: Parser a -> Parser a


---- Parsec implementation of the parser 
irParser :: Parser IRAST
irParser = undefined

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


