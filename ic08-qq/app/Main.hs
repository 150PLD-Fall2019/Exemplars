{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module Main where

import Arith


hours = 5

seconds = [arith| (* hours (* 60 60)) |]

main :: IO ()
main = putStrLn $ show seconds
