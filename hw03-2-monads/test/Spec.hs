module Main where
import Test.HUnit

-- run with `stack test`
main :: IO ()
main = runTestTT tests >> return ()

-- add labeled tests as you see fir
tests = TestList
          [TestLabel "test1" test1
          ,TestLabel "test2" test
          ]

-- define check and expect with your test values
test1 = 
  let check = undefined
      expect = undefined
  in  assertEqual "this test shows x" expect check 

-- repeat as necessary...
test2 = test1

 
