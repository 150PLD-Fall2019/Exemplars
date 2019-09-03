
import Test.HUnit
import Lib 

main :: IO ()
main = do
       runTestTT $
         TestList [TestLabel "name test" $ 
                             TestCase $
                               assertEqual "for my name" "mahrens" myName
                  ]
       return ()
