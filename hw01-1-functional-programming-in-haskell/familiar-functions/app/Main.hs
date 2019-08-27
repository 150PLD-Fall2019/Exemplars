module Main where

mainUnsugared :: IO ()
mainUnsugared = 
  (putStrLn "Enter your name") >>
  readLn >>= 
  (\name -> putStrLn $ "hello, " ++ name ++ "!")


main :: IO ()
main = undefined
