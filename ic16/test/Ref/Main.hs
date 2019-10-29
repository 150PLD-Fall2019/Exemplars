{-# LANGUAGE QuasiQuotes #-}-
module Main where

import IR

[ir|
  data E = 
      Lam String E
    | App E E 
    | Var String
    | Let String E E
  reducing
    (Let x e body) -> (App (Lam x body) e)
|] 


