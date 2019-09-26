module Arith where 
import Prelude hiding (log, print)
import Data.Char (isSpace)
data Exp = 
    Plus Exp Exp
  | Minus Exp Exp
  | Times Exp Exp
  | Div Exp Exp
  | Const Int
  deriving (Show,Eq)

data Hopefully a = Error String | OK a 
  deriving (Show, Eq)

instance Functor Hopefully where 
  fmap f (Error string) = Error string 
  fmap f (OK a)         = OK (f a)

instance Applicative Hopefully where 
  (OK f) <*> (OK a) = OK (f a) 
  (Error string) <*> (OK a) = Error string
  (OK f) <*> (Error string) = Error string 
  (Error string1) <*> (Error string2) = Error (string1 ++ "\n" ++ string2)
  pure = OK

instance Monad Hopefully where 
  return = OK
  (>>=) = ifOkThen

ifOkThen :: Hopefully a -> (a -> Hopefully b) -> Hopefully b
ifOkThen (Error string) f = Error string 
ifOkThen (OK a) f = f a

eval :: Exp -> Hopefully Int
eval (Plus e1 e2) = do
  i1 <- (eval e1) 
  i2 <- (eval e2) 
  return (i1 + i2) 
eval (Minus e1 e2) = do 
  i1 <- (eval e1)
  i2 <- (eval e2)
  return (i1 - i2) 
eval (Times e1 e2) = do
  i1 <- (eval e1)  
  i2 <- (eval e2) 
  return  (i1 * i2) 
eval (Div e1 e2)   = do 
  i1 <- (eval e1)
  i2 <- (eval e2) 
  if i2 == 0 then Error "Divide by Zero" else return (i1 `div` i2) 
eval (Const i) = return i

-- It's dangeorus to go alone, take this:
trimWhitespace :: String -> String
trimWhitespace s = 
  reverse (dropWhile isSpace $ reverse (dropWhile isSpace s))

-- YOUR TURN
-- 1) Define logger such that it accumulates a string log
--    and can produce a value a if queried.
data Logger a

-- 2) Implement a log api function that, given a string,
--    appends the string to your representation of the log
log :: String -> Logger ()
log  = undefined

-- 3) Instantiate Functor for your Logger which
-- maps a pure function over the "a" your Logger holds,
-- leaving the representation of the log alone
instance Functor Logger where
  fmap f logA = undefined

-- 4) Instantiate Applicative for your Logger which
-- produces a blank log for an "a" using `pure`
-- and can append two logged computations using `<*>`
instance Applicative Logger where
  pure a = undefined
  logFAB <*> logA = undefined

-- 5) Instantiate Monad for your Logger which
-- produces a blank log for an "a" using `return`
-- and can unwrap the a and a log from a Logger of "a",
-- apply a function "f" to "a" to get a Logger of "b",
-- and then consolidate the logs in a new Logger of "b"
instance Monad Logger where 
  return = pure 
  logA >>= faLogB = undefined 


evalL :: Exp -> Logger Int
evalL (Plus e1 e2) = do
  i1 <- evalL e1
  i2 <- evalL e2
  log "+"
  return (i1 + i2)
evalL (Minus e1 e2) = do
  i1 <- evalL e1
  i2 <- evalL e2
  log "-"
  return (i1 - i2)
evalL (Times e1 e2) = do
  i1 <- evalL e1
  i2 <- evalL e2
  log "*"
  return (i1 * i2)
evalL (Div e1 e2) = do
  i1 <- evalL e1
  i2 <- evalL e2
  log "div"
  return (i1 `div` i2)
evalL (Const i) = do
  log (show i)
  return i

-- 6) define a print function that returns the 
--    log of a computation

print :: Logger a -> String
print = undefined

-- 7) (OPTIONAL for testing) define a run function that
--    that evaluates the Logger expression and gets
--    the "a" from it

run :: Logger a -> a
run  = undefined


