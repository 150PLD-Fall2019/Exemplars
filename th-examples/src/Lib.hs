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

update :: Int -> Int -> Q Exp 
update n m = do
  tupNames <- mapM (newName . ("x" ++) . show) [1..n]
  updVarName <- newName "newValue"
  let vars = map VarP tupNames 
  return $ 
    LamE [TupP vars,VarP updVarName] $
     TupE $ map (\index -> 
                  if index == m then VarE updVarName else VarE $ tupNames !! index )
                [0..n-1]


delete :: Int -> Int -> Q Exp
delete n m = do
  tupNames <- mapM (newName . ("x" ++) . show) [1..n]
  return $ 
    LamE [TupP $ map VarP tupNames] $
      TupE $ 
        foldr (\index acc -> 
                 if index == m then acc else (VarE $ tupNames !! index) : acc)
              []
              [0..n-1]
              
swap :: Int -> Int -> Int -> Q Exp 
swap n i j = do
  tupNames <- mapM (newName . ("x" ++) . show) [1..n]
  return $ 
    LamE [TupP $ map VarP tupNames] $
      TupE $ 
        map (\index -> if index == i then VarE $ tupNames !! j
                       else if index == j then VarE $ tupNames !! i
                       else VarE $ tupNames !! index)
            [0..n-1]


   
