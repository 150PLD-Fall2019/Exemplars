{-#LANGUAGE TemplateHaskell #-}
import NCompose
import Test.HUnit

main :: IO ()
main = do
  runTestTT $ 
    TestList 
     [TestLabel "test1" test1
     ,TestLabel "test2" test2
     ] 
  return ()

compose2 :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
compose2 = $(ncompose 2)

compose2byHand :: (c -> d) -> (a -> b -> c)  -> (a -> b -> d)
compose2byHand g f = \a b -> g (f a b)

compose3 :: (d -> e) -> (a -> b -> c -> d) -> (a -> b -> c -> e)
compose3 = $(ncompose 3)

compose3byHand :: (d -> e) -> (a -> b -> c -> d) -> (a -> b -> c -> e)
compose3byHand g f = \a b c -> g (f a b c)

test1 = TestCase $ assertEqual "compose2" ((compose2byHand show (+)) 1 2) 
                               ((compose2 show (+)) 1 2)

test2 = 
  let f :: Int -> Int -> Int 
      f = (+)
  in  TestCase $ 
        assertEqual "compose3" ((compose3byHand (* 10) foldr) f 0 [1,2,3,4,5,6])
                               ((compose3 (* 10) foldr) f 0 [1,2,3,4,5,6])
        
