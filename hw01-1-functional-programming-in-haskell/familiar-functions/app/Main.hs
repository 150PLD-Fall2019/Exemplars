module Main where

mainUnsugared :: IO ()
mainUnsugared = 
  (putStrLn "Enter your name") >>
  getLine >>= 
  (\name -> putStrLn $ "hello, " ++ name ++ "!")


main :: IO ()
main = undefined
