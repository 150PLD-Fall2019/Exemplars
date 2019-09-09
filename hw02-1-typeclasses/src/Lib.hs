module Lib where
import Data.Word
import Data.Int
-- parametric polymorphism looks like:

myFst :: (a,b) -> a
myFst (a,b) = a 

-- ad-hoc polymorphism looks like:
--   (Show a) is a constraint that says type a must be statically known to
--   implement the methods of the Show typeclass (e.g. the function show)
myShow :: (Show a) => a -> String 
myShow = show

-- 1) use the methods of the Bounded typeclass to implement the following
-- functions

data Rect a = Rect {lower_left_coord :: (a,a), width :: a, height :: a}


largestAreaWord8Rectangle :: Rect Word8
largestAreaWord8Rectangle = undefined 

largestAreaInt16Rectangle :: Rect Int16
largestAreaInt16Rectangle = undefined



-- 2) use the methods of the Read and Show typeclass to implement the 
-- following functions

-- "1" `stringPlusInt "2" = "3" :: Int 
stringPlusInt :: String -> String -> String 
stringPlusInt x y = undefined

stringPlusWord :: String -> String -> String
stringPlusWord x y = undefined

-- 3) uncomment and define the type of the following function:
-- myfunction :: ???
myFunction x y z = ((read ((show x) ++ y)) :: Int) == z


-- 4) Instance Eq and Ord for Suite and Card such that
--     all Spades > Hearts > Diamonds > Clubs
--     and
--     ordering (1-13) hold within a suite
--       let 1 be ace, 11 be jack, 12 be queen, and 13 be king
--     let a card only be well-formed for Int arguements 1 - 13
data Suit = Hearts | Diamonds | Spades | Clubs 
data Card = Card Suit Int

instance Eq Suit where 
  suit1 == suit2 = undefined 

instance Eq Card where 
  card1 == card2 = undefined

instance Ord Suit where 
  compare suit1 suit2 = undefined

instance Ord Card where
  compare card1 card2 = undefined


-- 5) Instance Show for Suit and Card, pretty printing a card as:
--      e.g. "Jack of Clubs" for Card Clubs 11
--      use undefined or error "message" for ill-formed cards.







