module Main where

import IR

main = do
  putStrLn $ show $ irParse "reduce Exp where (Let x e body) -> (App (Lam x body) e)"
  putStrLn $ show $ irParse "data Conds = N Int | B Bool reducing (B True) -> N 1 | (B False) -> N 0"
  
