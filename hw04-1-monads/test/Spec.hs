import Test.HUnit

import  Lib

import Data.Array (Array)
import qualified Data.Array as Array

import Data.Tree (Tree(..))
import qualified Data.Tree as Tree

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Sequence (Seq, (<|))
import qualified Data.Sequence as Seq

import Data.Map (Map)
import qualified Data.Map as Map

main :: IO ()
main = runTestTT tests >> return ()

tests  = TestList [ TestLabel "test0" test0
                  , TestLabel "test1" test1
                  , TestLabel "test2" test2
                  , TestLabel "test3" test3 
                  , TestLabel "test4" test4 
                  , TestLabel "test5" test5
                  , TestLabel "test6" test6
                  , TestLabel "test7" test7
                  ]

test0  = 
  TestCase $ 
    assertEqual "fmap array" (Array.listArray (0,4) [False, True, False, True, False]) $ mapOverArray (Array.listArray (0,4) [True, False, True, False, True])

test1  = 
  TestCase $ 
    assertEqual "fmap tree" 
                (Node "1" [Node "2" [], Node "3" []]) $ 
                mapOverTree (Node 1 [Node 2 [], Node 3 []])
       

test2  = 
  TestCase $ 
    assertEqual "fmap Sequence" 
                (True <| True <| False <| False <| Seq.empty) 
                (mapOverSeq $ 255 <| 126 <| 125 <| 125 <| Seq.empty)


test3  = 
  TestCase $ 
    assertEqual "app Maybe" (Just "5") whatDoesThisExpDo0

test4  = 
  TestCase $ 
    assertEqual "app List" [1,2,3,2,3,4,3,4,5] whatDoesThisExpDo1

test5  = 
  TestCase $ 
    assertEqual "app Tree" 
     (Node True                 --       True
       [Node False [],          --  |     |      |  
        Node False [],          -- False False  False
        Node False [            --              |    |
          Node True [],        --            True  True
          Node True []]])
     whatDoesThisExpDo2


test6  = 
  TestCase $ 
    assertEqual "bind List" [2,6,10,14,4,12,20,28,6,18,30,42] 
                            (products 3 4)


test7 = 
  TestCase $
    assertEqual "state" 
      (3,Map.insert "x" 1 $ Map.insert "y" 2 $ Map.insert "z" 3 $ Map.empty)
      runYourStatefulProgram 
