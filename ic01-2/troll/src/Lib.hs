module Lib  where

import Control.Monad (mapM)
import qualified Data.Map as M
import Data.Map (Map)
 

newtype Val = Set [Int]

data Exp = 
 | If Exp Exp Exp 
 | Repeat Exp Exp 
 | String ::= Exp 
 | Seq Exp Exp 
 | V  Val 
 | D   Int Int     -- e.g. 3d6
 | Max Int Exp  -- max value of a roll
 | Sum Exp      -- add up a roll 
 | Int :#  Exp  -- build a multiset from n expressions 
 | Exp :@  Exp  -- multiset union 
 | Exp :+  Int  -- non-symetric addition
 | Int :<  Exp  -- filter predicates
 | Int :>  Exp
 | Int :<= Exp
 | Int :>= Exp 
 | Int :== Exp 
 | Int :/= Exp
 | Count Exp   -- Initial Basis Library
 | Pick Int Exp
type Env = Map String Val

data Random a = Random (Int -> (Int, a))

instance Functor Random where 
  fmap f (Random pipe) = 
    Random (\seed -> let (seed', a) = pipe seed in (seed', f a))

instance Applicative Random where 
  pure a = Random (\seed -> (seed + 13,a))
  (Random pipe1) <*> (Random pipe2) = 
    let pipe seed = 
          let (seed', f) = pipe1 seed
              (seed'',a) = pipe2 seed'
          in (seed'', f a)
    in Random pipe 

instance Monad Random where 
  return = pure 
  (Random pipe) >>= f = 
    let pipe' seed = 
         let (seed', a) = pipe seed 
             (Random pipe'') = f a
         in pipe'' seed'
    in Random pipe'

inRange :: Int -> Int -> Random Int
inRange lo hi = Random (\seed -> (seed,lo + (seed `mod` ((hi - lo) + 1))))  

runRandom :: Int -> Random a -> a 
runRandom seed (Random pipe) =
  let (_,a) = pipe seed
  in a

eval :: Env -> Exp -> Random (Env,Val) 
eval rho (If cond e1 e2) = do 
  (rho', Set vals) <- eval rho cond
  eval rho' $ 
    if   length vals > 0
    then e1 
    else e2
eval rho (Repeat exp cond) = do
  (rho', Set vals) <- eval rho cond
  if length cond > 0
  then do 
    eval rho' exp
    eval rho' (Repeat exp cond)
  else
    return $ (rho', AV $ Set []) 
eval rho (x ::= exp) = do
  (rho', val) <- eval rho exp
  return (M.insert x val rho', val)
eval rho (Seq exp1 exp2) = do 
  (rho', _) <- eval rho exp1 
  eval rho' exp2
eval rho (V val) = return $(rho, val)
eval rho (D n m) = 
  vals <- mapM (inRange 0 m))
  return (rho, Set avals)
eval rho (Max n e) = do 
  (rho', Set vs) <- eval rho e
  return (rho', Set [max vs])
eval rho (Sum e) = do
  (rho', Set vs) <- eval rho e
  return (rho', Set [foldr (+) 0 vs])
eval  rho ( 0 :# _) = return (rho, Set [])
eval rho ( n :# e) = do 
  (rho' , Set vals ) <- eval rho e
  (rho'', Set vals') <- eval rho' (n-1 :# e)
  return (rho'', Set $ vals ++ vals')
eval rho (exp1 :@ exp2) = do 
  (rho' , Set vals1) <- eval rho  exp1
  (rho'', Set vals2) <- eval rho' exp2 
  return $ (rho'', Set $ vals1 ++ vals2)
eval rho (e :+ n) = do
  (rho', Set vals) <- eval rho e
  return (rho', Set $ map (+ n) vals)
eval rho (n :< e) = do 
  (rho', Set vals) <- eval rho e
  return $ (rho', Set $ filter (\m -> n < m) vals)
eval rho (n :> e) = do 
  (rho', Set vals) <- eval rho e
  return $ (rho', Set $ filter (\m -> n > m) vals)
eval rho (n :<= e) = do 
  (rho', Set vals) <- eval rho e
  return $ (rho', Set $ filter (\m -> n <= m) vals)
eval rho (n :>= e) = do 
  (rho', Set vals) <- eval rho e
  return $ (rho', Set $ filter (\m -> n >= m) vals)
eval rho (n :== e) = do 
  (rho', Set vals) <- eval rho e
  return $ (rho', Set $ filter (\m -> n == m) vals)
eval rho (n :/= e) = do 
  (rho', Set vals) <- eval rho e
  return $ (rho', Set $ filter (\m -> n /= m) vals)
eval rho (Count e) = do
  (rho', Set vals) <- eval rho e
  return (rho', Set $ [length vals])
eval rho (Pick 0 _) = do
  (rho', _) <- eval rho e
  return (rho', Set [])
eval rho (Pick n e) = do
  (rho', Set vals) <- eval rho e
  case vals of
  [] -> (rho', Set [])
  _  -> do 
    index <- inRange 0 (length vals - 1)
    case splitAt index vals of
      (xs,[])      -> (rho', Set xs) -- impossible by range 0 ... length - 1
      (xs, y : ys) -> do
        (rho'', Set zs) <- eval rho' (Pick (n - 1) (V $ Set $ xs ++ ys))
        return (rho'', Set (y : zs))

