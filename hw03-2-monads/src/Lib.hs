module Lib  
( )
where
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer

-- Note: if an expression is left undefined here, 
--       but is defined in the video
--       please enter it in as you follow along.

-- review: map
myMap :: (a -> b) -> [a] -> [b]
myMap f [] = []
myMap f (x : xs) = (f x) : (myMap f xs)

------
-- Functors --
data Trivial a = Trivial a
  deriving (Show)
-- A function to "map" over values of a stores in the Trivial data structure
myTrivialMap :: (a -> b) -> Trivial a -> Trivial b
myTrivialMap f (Trivial a) = undefined 

instance Functor Trivial where
  fmap = myTrivialMap 


--
-- 1) Use fmap (infix operator <$>) to define the following functions
--    do not use pattern matching as in the laws

-- algebraic laws: 
--   cautiousPlus1 (Just n) = Just (n + 1)
--   cautiousPlus1 Nothing  = Nothing
cautiousPlus1 :: Maybe Int -> Maybe Int 
cautiousPlus1 = undefined

-- algebraic laws:
--   failableShowInt (Right 0xabad1dea) = Right "hacker voice: I'm in"
--   failableShowInt (Right n)          = Right "n"
--   failableShowInt (Left msg)         = Left msg
failableShow :: (Either String) Int -> Either String String 
failableShow = undefined

-- MyMaybe :: * -> *
data MyMaybe a = MyJust a | MyNothing 

-- MyEither :: * -> * -> *
data MyEither a b = MyLeft a | MyRight b


data Trivial2 t1 t2 = Trivial2 t2
  deriving (Show)

instance Functor (Trivial2 t1) where 
  fmap f (Trivial2 a) = undefined
 



-- 2) Instantiate Functor for MyMaybe and MyEither
--    headers intentionally left blank for you to write


-- Revisit: Logger
data Logger a = Logger String a 
  deriving (Show)
instance Functor Logger where 
  fmap f (Logger msg a) = undefined

data LoggerF a = LoggerF (String -> (String, a))

runLoggerF :: LoggerF a -> (String -> (String,a))
runLoggerF (LoggerF fn) = fn 



-- 3) Instantiate Functor for LoggerF such that the laws hold
-- algebraic laws 
--   runLoggerF (f <$> LoggerF g) s  = 
--     ("applied a function to " ++ (fst . g) s, (f . snd . g) s) 

instance Functor LoggerF where 
 fmap f (LoggerF g) = undefined


------
-- Applicatives -- 
-- 

liftTrivial :: a -> Trivial a 
liftTrivial = undefined

liftTrivialF :: Trivial (a -> b) -> Trivial a -> Trivial b
liftTrivialF (Trivial f) (Trivial a) = undefined

instance Applicative Trivial where
  pure = liftTrivial
  (<*>) = liftTrivialF 

-- 4) Define the following function using <$> <*> and pure
--    do not use pattern matching

-- algebraic laws
-- Just 1 +? Just 2   = Just 3
-- Nothing +? n       = Nothing
-- n       +? Nothing = Nothing
(+?) :: Maybe Int -> Maybe Int -> Maybe Int
x +? y = undefined 

-- 5) Instantiate Applicative for MyMaybe and Logger 
--   

-- for MyMaybe, generalize the algebraic laws for +? to work for any function





-- for Logger, the algebraic laws look exactly like the code you will write,
-- but the semantics are that it should append the string arguments together.
-- play with the  applicative methods on the type (Monoid m => (,) m a)
-- e.g. ``("hello ",(+1)) <*> (" world",2)`` 




------ 
-- Monads --
--

-- Monads that expose their algebraic data type's data constructors
makeAMaybeMonad :: a -> Maybe a
makeAMaybeMonad = Just 

makeAnEitherMonad :: a -> (Either b) a
makeAnEitherMonad = Right


-- Monads that provide an abstract data type (or an opaque API)
makeAReaderMonad :: a -> Reader s a
makeAReaderMonad = (\x -> return x)

makeAReaderMonad2 :: a -> Reader s s
makeAReaderMonad2 = (\_ -> ask) 

makeAWriterMonad :: (Monoid s) => a -> (Writer s) a
makeAWriterMonad = (\a -> return a)

makeAWriterMonad2 :: (Monoid s) => s -> Writer s ()
makeAWriterMonad2 = (\s -> tell s)

makeAStateMonad :: s -> State s ()
makeAStateMonad = (\s -> put s)

makeAStateMonad2 :: a -> State s s
makeAStateMonad2 = (\_ -> get)


-- 6) 
--    Type and define a function `tracer` that prints its argument before 
--    returning it.
--    (uncomment the type after defining it)
-- tracer :: undefined
tracer a = undefined



instance Monad Trivial where
  return = undefined
  (>>=) (Trivial a) f = undefined


-- 7) Instantiate Monad for MyMaybe and MyEither
 





-- Consider the definition and typeclass instances for the Seeded datatype

data Seeded a = Seeded (Int -> (Int, a))

magicNumber :: Int 
magicNumber = 4 -- https://xkcd.com/221/
    
instance Functor Seeded where                                     
  fmap f (Seeded pipe) =                                      
     Seeded (\seed -> 
             let (seed', a) = pipe (seed + magicNumber) 
             in (seed', f a))                

instance Applicative Seeded where                                   
  pure a = Seeded (\seed -> (seed + magicNumber,a))                      
  (Seeded pipe1) <*> (Seeded pipe2) = 
     let pipe seed =                                
           let (seed', f) = pipe1 $ seed + magicNumber     
               (seed'',a) = pipe2 $ seed'+ magicNumber        
          in (seed'', f a)                                      
    in Seeded pipe                     
instance Monad Seeded where                                              
  return = pure                                                         
  (Seeded pipe) >>= f =                                                 
    let pipe' seed =                                         
         let (seed', a) = pipe (seed + magicNumber)          
             (Seeded pipe'') = f a                         
         in pipe'' (seed' + magicNumber)                        
    in Seeded pipe'    

-- 8) implement the following api functions for Seeded
--
-- algebraic law
--   inRange n m = x where n <= x <= m
inRange :: Int -> Int -> Seeded Int
inRange n m = undefined

runSeeded :: Int -> Seeded a -> a
runSeeded seed (Seeded pipe) =
  let (_,a) = pipe seed
  in a

getSeed :: Seeded Int
getSeed = Seeded (\seed -> (seed,seed))

-- 9) design and implement a datatype Distributed that stores
--    values of a the probabilities that they are likely to happen
--    (think the semantics of troll)
--    a. Define the right hand side of the datatype
--    b. Define the Functor, Applicative, and Monad instances
--    c. Implement a subset of the api functions (Inspired by troll)

-- data Distributed a = ...
data Distributed a

instance Functor Distributed where
  fmap = undefined

instance Applicative Distributed where
  pure = undefined
  (<*>) = undefined

instance Monad Distributed where
  return = undefined
  (>>=)   = undefined

-- constructors

-- Where the probablity of no value is 100%
unlikely :: Distributed a
unlikely = undefined

-- guaranteed value smart constructor
lift :: a -> Distributed a
lift = undefined

-- distribution smart constructor
-- We will be using the Rational Datatype to represent raw probabilities
-- ensure that the values add up to 1/1

liftD :: [(Rational, a)] -> Distributed a
liftD = undefined

-- lower functions
-- the function parameter expects its argument to be ordered by likelyhood
--   with the most likely element being at the head of the list.
chooseBy :: ([a] -> a) -> Distributed a -> Maybe a
chooseBy = undefined

-- choose uniformally randomly from the values distributed by the parameter
-- weighted by their distribution probabilities.
choose :: Distributed a -> Maybe a
choose = undefined

-- 10) Design and implement 
--     2 api functions you think might be useful for the Distributed
--     datatype.


-- 11) Please write unit tests for the exported functions
--     for the Distributed datatype's api functions in Spec/test.hs
--     -- Note: Please do not let the data constructor you define get exported.

  
