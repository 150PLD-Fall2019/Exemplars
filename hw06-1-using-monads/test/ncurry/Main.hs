{-#LANGUAGE TemplateHaskell #-}
import NCurry
import Test.HUnit

main :: IO ()
main = do
  runTestTT $ 
    TestList 
     [TestLabel "test1" test1
     ,TestLabel "test2" test2
     ] 
  return ()

curry3 :: ((a,b,c) -> d) -> a -> b -> c -> d
curry3 = $(ncurry 3)

curry3byHand :: ((a,b,c) -> d) -> a -> b -> c -> d
curry3byHand f = \a b c -> f (a,b,c)

curry4 :: ((a,b,c,d) -> e) -> a -> b -> c -> d -> e
curry4 = $(ncurry 4)

curry4byHand :: ((a,b,c,d) -> e) -> a -> b -> c -> d -> e
curry4byHand f = \a b c d -> (f (a,b,c,d))

test1 = 
  TestCase $ 
    assertEqual "curry3" 
                (curry3byHand (\(x,y,z) -> (x + y) `div` z) 10 15 4) 
                (curry3 (\(x,y,z) -> (x + y) `div` z) 10 15 4)

test2 = 
  let f :: (Int,Int,Int,Bool) -> Int
      f (x,y,z,cond) = 
        if cond
        then x + y
        else x + z
  in  TestCase $ 
        assertEqual "curry4" 
                       (curry4byHand f 10 20 30 True)
                       (curry4 f 10 20 30 True) 
