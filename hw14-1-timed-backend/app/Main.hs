{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Main where

import Lib

srcDriver :: IO [Int]
srcDriver = readPin 1 >>= \i -> return [i]

snkDriver :: [Int] -> IO ()
snkDriver xs = writePin 2 $ if any (== 0) xs then 0 else 1

myProg = $(
  let 
    setup1 = 
      (timedPinMode 1 Read  50) :>>
      (timedPinMode 2 Write 500) :>>
      (initQueue "x" 10 0) 
    loop1 = 
      mark 2 :>>
      (("x",10) :->> 'snkDriver) :>>
      (foldr1 (:>>) $ 
        replicate 10 $
          mark 1 :>> 
          ("x" :<<- 'srcDriver) :>>
          (delay 50))
  in  timed setup1 loop1)

main :: IO ()
main = myProg

badProg = $(  
 let 
    setup1 = 
      (timedPinMode 1 Read  10) :>>
      (timedPinMode 2 Write 500) :>>
      (initQueue "x" 10 0) 
    loop1 = 
      mark 2 :>>
      (("x",10) :->> 'snkDriver) :>>
      (foldr1 (:>>) $ 
        replicate 10 $
          mark 1 :>> 
          ("x" :<<- 'srcDriver) :>>
          (delay 100))
  in  timed setup1 loop1)

