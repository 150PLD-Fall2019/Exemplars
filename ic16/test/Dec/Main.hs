module Main where

import IR
data E = 
    Lam String E
  | App E E
  | Var String
  | Let String E E 

[ir| reduce E where (Let x e body) -> (App (Lam x body) e) |]

