module Lib where
import Control.Monad.Trans.Maybe -- used in part 4
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State -- used in part 6

-- 1) Recall the warmup activity where we wanted to mix the IO and Maybe monads:
--    consider a similar computation, `addIO`


addIO :: IO (Maybe Int)
addIO = do
  x <- getLine 
  y <- getLine
  case (readMaybe x :: Maybe Int, readMaybe y :: Maybe Int) of 
    (Just n, Just m) -> return $ Just (n + m)
    _                -> return Nothing

-- Aside: Notice how we have to "construct" the value using Just or Nothing to 
-- get a "Maybe Int" and then "construct" the value again using IO's return
-- equivalently, we could have written: 
addIO' :: IO (Maybe Int)
addIO' = do
  x <- getLine 
  y <- getLine
  case (readMaybe, readMaybe y) of 
    (Just n, Just m) -> return $ return (n + m)
    _                -> return Nothing
-- type inference + type classes let the inner `return` be the `return` statically
--   associated with `Maybe`
-- and the outter `return` be the `return` method statically associated with IO




-- 2) Your Turn:
--    write an IO () program that calls addIO twice and then prints the sum.
--    it should print "oops" if any of the numbers entered were malformed.


addFourNums :: IO ()
addFourNums = do









-- 3) Key idea: Notice how we had to unwrap the intermediate "Maybe values" again in 
--    `addFourNums`. 
--    Hypothesis: 
--      It would be nice if there was a way to abstract away
--      the unwrapping and rewrapping of Maybe, like >>= and return do for Maybe
--      AND YET still be able to do IO

-- Proposed Solution: the type `MaybeT (m :: * -> *) a`.
--    Aside: lift here is not template haskell lift. it is a poorly chosen name
--           for the typeclass `MonadTrans` that means:
--           - perform the monadic computation for `m` here.
--           in this case: `lift :: IO a -> MaybeT IO a`
--    Aside2: Most monad transformers are written where the simpler one (e.g. Maybe)
--            is written in terms of the higher order one (e.g. MaybeT)
--              just like you can define `map` in terms of `fold` in COMP105
--            Maybe came first, however, so we need a helper function to make
--            life easier: `liftMaybe`
liftMaybe :: (Monad m) => Maybe a -> MaybeT m a
liftMaybe maybeA = MaybeT (return maybeA)
-- if you want to understand why this works, compare it to the definition of
--   MaybeT: `data MaybeT (m :: * -> *) a = MaybeT { runMaybeT :: (m (Maybe a)) }
--     the `return` in `liftMaybe` wraps the `Maybe a` in the monadic type,
--     then the `MaybeT` data constructor wraps that in the MaybeT type

-- 4) Consider this solution in action for a rewrite of addMaybeTIO
addMaybeTIO :: MaybeT IO Int 
addMaybeTIO = do
  x <- lift getLine
  y <- lift getLine
  n <- liftMaybe $ readMaybe x
  m <- liftMaybe $ readMaybe y
  return $ n + m

-- Intuition
-- In the same way we have "higher-order functions" that can take, and produce
--   functions, the type constructor `MaybeT` is a "higher-order monad"
-- MaybeT is defined such that
--   give it a Monad called `m` and it will produce (or act like a monad) 
--   called `MaybeT m`

-- 5) Recall, addFourNums does something interesting in the Nothing case
--     so we cannot rewrite it interms of `MaybeT IO`
--    To get the resulting IO (Maybe Int) `addFourNums` needs
--    we can use the record function `runMaybeT`.
--    `runMaybeT :: MaybeT m a -> m (Maybe a)`
--    Yourturn: rewrite addFourNums as addFourNums' using addMaybeTIO instead

addFourNums' :: IO ()
addFourNums' = do










-- Pro-Tip: 
--   Typically, we want to save "running" or "collapsing" the "Monad Stack"
--   until the last possible moment.


-- 6) Your Turn: 
--    You can nest transformers inductively this way.
--    Write a program `lookupIO` that: 
--      i) asks the user for a variable name
--      ii) looks up the integer associated with that variable in an environment `Map String Int`
--          which is stored in state
--      iii) but represents the variable not being in the state with Nothing and
--           the variable being in the state with (Just value)
--    helpful functions:
--      lift       :: (MonadTrans t, Monad m) => t m a -> m a 
--        - note StateT and MaybeT instance MonadTrans
--      return     :: (Monad m) => a -> m a
--      liftMaybe  :: (Monad m) => Maybe a -> MaybeT m a
--      get        :: MaybeT s m s
--      Map.lookup :: k -> Map k v -> Maybe v
lookupIO :: (StateT (Map String Int) (MaybeT IO) Int)
lookupIO = do
















