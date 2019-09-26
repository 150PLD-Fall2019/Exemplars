import Test.HUnit
import Arith
import Prelude hiding (print)

main :: IO ()
main = do
  runTestTT tests
  return ()

tests = 
  TestList 
    [TestLabel "test1" test1
    ,TestLabel "test2" test2
    ,TestLabel "test3" test3
    ]

test1 = TestCase $
  assertEqual "const" "1" (print $ evalL (Const 1))

test2 = TestCase $ 
  assertEqual "plus" 
    "1 2 +" 
    (print $ evalL (Plus (Const 1) (Const 2)))

test3 = TestCase $
  assertEqual "all of 'em"
    "1 2 + 4 3 - * 5 div"
    (print $ evalL 
      (Div 
        (Times 
          (Plus (Const 1) (Const 2)) 
          (Minus (Const 4) (Const 3))) 
        (Const 5)))

