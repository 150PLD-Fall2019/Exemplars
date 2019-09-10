module Lib  where
-- library imports 

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Array as Array
import Data.Array (Array)
import qualified Data.Tree as Tree
import Data.Tree (Tree(..))
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import Data.Word 
import Data.Char

import Control.Monad.Trans.State
----
-- Functors

myMap :: (a -> b) -> [a] -> [b]
myMap f [] = []
myMap f (x : xs) = f x : myMap f xs 

-- instance Functor (f :: * -> *) where
--   fmap :: (a -> b) -> f a -> f b

-- some data structures

mapOverMaybe1 :: Maybe Int
mapOverMaybe1 = fmap (+ 1) (Just 3)

mapOverMaybe2 :: Maybe Int
mapOverMaybe2 = fmap (+ 1) Nothing 

mapOverMaybe3 :: Maybe Int
mapOverMaybe3 = (+ 1) <$> (Just 2)

mapOverEither1 :: (Either String) Int
mapOverEither1 = fmap (+ 1) (Right 3)

mapOverEither2 :: Either String Int
mapOverEither2 = fmap (+ 1) (Left "error case being thread through")

mapOverTuple :: (Int, String)
mapOverTuple = fmap ("Hello " ++) (1,"Matt")

data AList k v = AList [(k,v)]
  deriving (Show, Eq)
instance Functor (AList k) where
  fmap f (AList assocList) = 
    let fmap' [] = []
        fmap' ((key,value) : rest) = (key, f value) : (fmap' rest)
    in AList $ fmap' assocList 


mapOverAList :: AList Int String
mapOverAList = fmap ("Hello, " ++) $ AList [(1,"Matt"),(2,"Kathleen"),(3,"Karl")]

mapOverMap :: Map Int String 
mapOverMap = 
  fmap ("Hello" ++) $ 
        Map.insert 1 "Matt" $ 
        Map.insert 2 "Kathleen" $ 
        Map.insert 3 "Karl" $ 
        Map.empty 


-- 1) Implement the following mapOver expressions for some not-so-common data structures
-- What they should produce is in test/Spec.hs

-- Arrays: http://hackage.haskell.org/package/array-0.5.3.0/docs/Data-Array.html
mapOverArray :: Array Int Bool -> Array Int Bool
mapOverArray = undefined 

-- Trees: http://hackage.haskell.org/package/containers-0.6.2.1/docs/Data-Tree.html
mapOverTree :: (Show a) => Tree a -> Tree String 
mapOverTree = undefined 

-- Sets: http://hackage.haskell.org/package/containers-0.6.2.1/docs/Data-Set.html
mapOverSet :: (Int -> Int) -> Set Int 
mapOverSet = undefined

-- Sequences: http://hackage.haskell.org/package/containers-0.6.2.1/docs/Data-Sequence.html
mapOverSeq :: Seq Word8 -> Seq Bool
mapOverSeq = undefined

----
-- Applicatives

maybePlus1 :: Maybe (Int -> Int)
maybePlus1 = (+) <$> Just 1

maybe2 :: Maybe Int 
maybe2 = maybePlus1 <*> (Just 1)

maybeNothing :: Maybe Int
maybeNothing = maybePlus1 <*> Nothing

eitherFailedEarly :: Either String (Int -> Int -> Int)
eitherFailedEarly = Left "oops!"

compStopShort :: Either String Int 
compStopShort = eitherFailedEarly <*> (Right 1) <*> (Right 2)

accumulatingTuple :: (String, Int)
accumulatingTuple = (" + ", (+)) <*> (" 1 ", 1) <*> (" 2 ", 2)

 
-- 2) Instantiate the following data structures such that the unit tests (in test/Spec.hs) on their corresponding expressions pass.
--

someMaybe :: Maybe Int
someMaybe = undefined

whatDoesThisExpDo0 :: Maybe String
whatDoesThisExpDo0 = (Just show) <*> someMaybe

someList :: [Int]
someList = undefined

whatDoesThisExpDo1 :: [Int]
whatDoesThisExpDo1 = [(+1), (+2), (+3)] <*> someList

someTree :: Tree Bool
someTree = undefined

whatDoesThisExpDo2 :: Tree Bool
whatDoesThisExpDo2 = Node id [Node not []] <*> someTree


----
-- Monads

-- The constructors you could unwrap, but...
just :: a -> Maybe a
just = Just

tryDiv :: Int -> Int -> Maybe Int
tryDiv x 0 = Nothing
tryDiv x y = Just (x `div` y)

right :: b -> Either a b
right = Right

checkPass :: String -> Either String Int
checkPass pass 
  | length pass >= 8 && 
  any isNumber pass = Right $ length $ filter isPunctuation pass 
  | length pass < 8   = Left "please make a password of at least 8 characters"
  | otherwise         = Left "please include a number in your password"

showStrength :: Int -> Either String String
showStrength s 
  | s > 30 = Left "Are you trying to cross site script us through your password?"
  | s > 5 = Right "Strong"
  | s < 2 = Right "Weak"
  | otherwise = Right "Medium"
 
bindThemWithPatternMatching :: Either String String
bindThemWithPatternMatching = 
  case checkPass "password" of
    Left err -> error err
    Right strength ->
      case showStrength strength of
        Left err -> error err
        rightMsg -> rightMsg

bindThemWithBind :: Either String String
bindThemWithBind = (checkPass "R3a!!yS3cuR3P@$$w0r@'") >>= showStrength

bindThemWithDo :: Either String String
bindThemWithDo = do
  strength <- checkPass ":(){ :|:& };: :(){ :|:& };: :(){ :|:& };:"
  strengthText <- showStrength strength
  return $ "Your password strength is : " ++ 
           (show strength) ++ " which is considered " ++ 
           strengthText 

-- 3) Write a function that produces the n first even numbers starting at 2
nEvens :: Int -> [Int]
nEvens n = undefined

-- 4) Write a function that produces the n first odd numbers starting at 1
nOdds :: Int -> [Int]
nOdds n = undefined

-- 5) Write a function that given two integers, n and m, returns a list of all products (multiplication) of the first n even numbers and the first m odd numbers.
-- Either use bind or do notation

products :: Int -> Int -> [Int]
products n m = undefined

-- the smart constructors that hide the implementation
--

myGet :: State s s
myGet = get

myPut :: s -> State s ()
myPut = put

statefulProgram :: State (Int,Int) Int
statefulProgram = do
  (m,n) <- get 
  case (m,n) of
    (0,_)         -> return $ n+1
    (m,0) | m > 0 -> do
      put (m-1,1)
      statefulProgram
    _             -> do
      put (m, n-1)
      partialResult <- statefulProgram
      put (m-1, partialResult)
      statefulProgram

runStatefulProgram :: Int
runStatefulProgram = fst $ runState statefulProgram (2,2)

-- 6) Implement the following stateful program so that the unit tests pass and the value in the state are used.

-- stateful version of "x" + "y" as if the state was a value environment
yourStatefulProgram :: State (Map String Int) Int
yourStatefulProgram = undefined

runYourStatefulProgram = fst $ runState yourStatefulProgram (Map.insert "x" 1 $ Map.insert "y" 2 $ Map.empty)


-- IO
imperativeProgram :: IO Int 
imperativeProgram = do
  x <- getLine
  y <- getLine 
  putStrLn $ "received " ++ x ++ " and " ++ y
  return $ (length x) + (length y)

-- 7) Implement your imperative program such that it accumulates the numbers the user inputs (converting strings to numbers using read) and finally prints out their sum once the user enters "Q"
yourImperativeProgram :: IO ()
yourImperativeProgram = undefined


