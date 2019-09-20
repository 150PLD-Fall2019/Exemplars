{-#LANGUAGE TemplateHaskell #-}
import Arith
import Test.HUnit

main :: IO ()
main = do
  runTestTT $ 
    TestList 
     [TestLabel "test1" test1
     ,TestLabel "test2" test2
     ,TestLabel "test3" test3
     ,TestLabel "test4" test4
     ,TestLabel "test5" test5
     ,TestLabel "test6" test6 
     ] 
  return ()

test1 = 
  TestCase $ 
    assertEqual "const" 
                (Const 110) $
                parseArith "110"
test2 = 
  TestCase $ 
    assertEqual "+" 
                (Plus (Const 1) (Const 2)) $
                parseArith "(+ 1 2)"
test3 = 
  TestCase $ 
    assertEqual "-" 
                (Minus (Const 5) (Const 3)) $
                parseArith "(- 5 3)"
test4 = 
  TestCase $ 
    assertEqual "*" 
                (Times (Const 6) (Const 6)) $
                parseArith "(* 6 6)"
test5 = 
  TestCase $ 
    assertEqual "/" 
                (Div (Const 7) (Const 0)) $
                parseArith "(/ 7 0)"
test6 = 
  TestCase $ 
    assertEqual "composition" 
                (Plus (Times (Const 1) (Const 2)) 
                      (Div (Const 4) 
                           (Minus (Const 5) (Const 1)))) $
                parseArith "(+ (* 1 2) (/ 4 (- 5 1)))"






