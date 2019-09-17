{-#LANGUAGE QuasiQuotes, TemplateHaskell #-}
module Main where

import Lib
-- find . -name "*.dump-splices"


sel5at3 = $(sel 5 3)

ex1 = sel5at3 (1,2,3,4,5)

sel62at33 = $(sel 62 33)


update5at2 = $(update 5 2)

ex2 = update5at2 (1,2,3,4,5) 100


del6at4 = $(delete 6 4)

ex3 = del6at4 (1,2,3,4,5,6)

swap6with2for4 = $(swap 6 2 4)

ex4 = swap6with2for4 (1,2,3,4,5,6) 

main :: IO ()
main = undefined
