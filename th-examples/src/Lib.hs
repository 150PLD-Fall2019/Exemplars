module Lib where

import Control.Monad (mapM, foldM)
import Language.Haskell.TH

sel :: Int -> Int -> Q Exp
sel n m = do
  tupNames <- mapM (newName . ("x" ++) . show) [0..n-1]
  let vars = map VarP tupNames
  return $ 
    LamE [TupP vars] $
      VarE $ tupNames !! m

-- Generates a function with a type like
-- (t1,..,tm,..,tn) -> tm -> (t1,..,tm,..,tn)
update :: Int -> Int -> Q Exp 
update n m = undefined 

-- Generates a function with a type like
-- (t1,..,tm,..,tn) -> (t1,..,tn)
delete :: Int -> Int -> Q Exp
delete n m = undefined 

-- Generates a functon with a type like
-- (t1,..,ti,..,tj,..tn) -> (t1,..,tj,..,ti,..,tn)
swap :: Int -> Int -> Int -> Q Exp 
swap n i j = undefined 




    
