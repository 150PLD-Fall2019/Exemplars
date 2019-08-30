import Test.HUnit

import qualified Lib as L

main :: IO ()
main = runTestTT tests >> return ()

tests  = TestList [ TestLabel "test1" test1
                  , TestLabel "test2" test2
                  , TestLabel "test3" test3
                  , TestLabel "test4" test4
                  , TestLabel "test5" test5 
                  , TestLabel "test6" test6 
                  , TestLabel "test7" test7
                  , TestLabel "test8" test8
                  , TestLabel "test9" test9
                  , TestLabel "test10" test10
                  ]

test1  = 
  TestCase $ 
    assertEqual "head of a list" (head [1,2,3])  (L.head [1,2,3])

test2  = 
  TestCase $ 
    assertEqual "tail of a list" (tail [1,2,3]) (L.tail [1,2,3])

test3 =  
  let even :: Int -> Bool
      even x = (x `mod` 2) == 0
  in  TestCase $ 
        assertEqual "filter a list" (filter   even [1,2,3,4,5,6]) 
                                    (L.filter even [1,2,3,4,5,6])

test4  = 
  TestCase $ 
    assertEqual "map over a list" (map   (+ 1) [1,2,3,4,5]) 
                                  (L.map (+ 1) [1,2,3,4,5])

test5  = 
  TestCase $ 
    assertEqual "fold over a list" (foldr   (+) 0 [1,2,3,4,5]) 
                                   (L.foldr (+) 0 [1,2,3,4,5])

test6  = 
  TestCase $ 
    assertEqual "append two lists" ([1,2,3] ++ [4,5,6]) ([1,2,3] L.++ [4,5,6])

test7  = 
  TestCase $ 
    assertEqual "addition over Maybe Ints" (Just 3) ((Just 1) L.+? (Just 2))

test8  = TestCase $ assertEqual "first of a pair"  1 (L.fst (1,2))

test9  = TestCase $ assertEqual "second of a pair" 2 (L.snd (1,2))

test10 = 
  TestCase $ assertEqual "testing the logger api" 
     ("(applying (applying + to 1) to 2)", 3) 
     (L.lower (L.app (L.app (L.lift "+" (+)) (L.lift "1" 1)) (L.lift "2" 2)))
 
