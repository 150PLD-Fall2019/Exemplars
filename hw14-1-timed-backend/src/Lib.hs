{-# LANGUAGE TemplateHaskell, QuasiQuotes, DeriveLift #-}
module Lib where

import qualified Data.Map as M
import Data.Map (Map)
import Control.Concurrent
import Language.Haskell.TH
import Language.Haskell.TH.Lift
import System.Timeout
import System.IO
import Data.IORef 
import Control.Monad (forever, when)
import Data.Maybe (isJust)
import Debug.Trace
-- standard library
writePin :: Int -> Int -> IO ()
writePin pin x = putStrLn $ "pin: " ++ (show pin) ++ " => " ++ show x

readPin :: Int -> IO Int
readPin pin =  
  do
    hSetBuffering stdin NoBuffering
    hSetEcho stdout False
    rdy <- hReady stdin
    if rdy 
    then do 
      c <- getChar
      return $ if ([c] == (show pin)) then 1 else 0
     else return $ 0


-- `Timed` is a small expression language for timed IO computation

data Timed =
    InitQ String Int Int -- initialize a queue with a name, capacity, and initial value
  | TPM Int PinMode Milliseconds  -- initialize a pin with a mode and a period
  | Delay Int                     -- delay primitive we can track
  | Mark Int                      -- Mark a pin invokation
  | (String,Int) :->> Name -- read from queue into named IO computation
  | String :<<- Name        -- write to queue from named IO computation
  | Timed :>> Timed -- sequence operator
  deriving (Lift) 

data PinMode = Read | Write
  deriving (Lift)

type Milliseconds = Int

-- smart constructors
timedPinMode :: Int -> PinMode -> Milliseconds -> Timed
timedPinMode = TPM

initQueue :: String -> Int -> Int -> Timed
initQueue = InitQ 

delay :: Milliseconds -> Timed
delay = Delay

mark :: Int -> Timed
mark = Mark

-- whole program analyses

-- allQs walks a program to grab all the queue names
allQs :: Timed  -> [String]
allQs (InitQ x _ _) = [x]
allQs (t1 :>> t2) = allQs t1 ++ allQs t2 
allQs _ = []

-- allPins walks a program to grab all the declared pin #s
allPins :: Timed  -> Map Int Int
allPins (TPM pin _ ms) = M.singleton pin ms
allPins (t1 :>> t2) = allPins t1 `M.union` allPins t2
allPins _ = M.empty

-- the primary static analysis for this language:
--   given a timeEnv that maps pins to their declared rates
--   and a program :: Timed
--   return a function that, given an initial allottment of time for each pin
--   will walk one iteration of the program and simulate the time spent
--   returning the resulting allottment after spending that time
analyze :: Map Int Int -> Timed  -> Map Int Int -> Maybe (Map Int Int)
analyze timeEnv (Mark pin) allotted = do -- Marking a pin "refuels" time allotment
  n <- M.lookup pin timeEnv
  return $ M.insert pin n allotted
analyze _ (Delay n) allotted = return $ M.map (\a -> a - n) allotted -- delaying a pin "spends" time
analyze timeEnv (t1 :>> t2) allotted =       -- sequencing side effects
  (analyze timeEnv t1 allotted) >>= (analyze timeEnv t2)
analyze timeEnv _ allotted = return allotted -- assume all other operations take negligable time

-- generate a custom runtime system based on the input program
eval :: Timed -> Q Exp
eval (InitQ x n v) = [| \store -> modifyIORef store (M.insert x (replicate n v)) |]
eval (TPM pin mode ms) = [| \store -> return () |] -- no runtime behavior for now
eval (Delay n) = [| \store -> threadDelay $ n * (10^3) |]
eval (Mark n) = [| \store -> return () |] -- no runtime behavior for now
eval ((x,n) :->> snk) = 
  [| \store -> do 
       env <- readIORef store
       case M.lookup x env of
         Nothing -> return $ error "runtime error: referenced unbound variable"
         Just vs -> do 
           $(return $ VarE snk) $ take n vs
           writeIORef store $ M.insert x (drop n vs) env
  |]
eval (x :<<- src) = 
  [| \store -> do 
       vs <- $(return $ VarE src)
       modifyIORef store (M.adjust (++ vs) x)
  |]
eval (t1 :>> t2) = 
  [| \store ->
       $(eval t1) store >> $(eval t2) store
  |]


-- metaprogram
--   given a setup and a loop
--   check the loop with respect to the pins declared in the setup
--   naive analysis algorithm: assert (time needed - time spent) in the loop = 0
timed :: Timed  -> Timed -> Q Exp
timed setup loop = do 
    let timeEnv = allPins setup -- pin time environment (statically declared periods for each pin)
    when (isJust $ do -- error when a pin does not use exactly its allotted time 
            allotted <- analyze timeEnv -- the statically determined time environment maps pins to periods 
                        loop    -- the loop body which is expected to contain delays and marks
                        (M.map (const 0) timeEnv) -- an initial time alottment of 0 for each pin
            if any (/= 0) $ M.elems allotted
            then Just ()
            else Nothing) 
         (reportWarning "A pin did not use exactly its alloted time") -- TODO improvement: better error messaging; Potential Design Question: Should the DSL stop compilation?
    -- runtime system
    [| do
         store <- newIORef $ foldr ((flip M.insert) [])  M.empty $ allQs setup -- queue value environment
         $(eval setup) store
         forever $ $(eval loop) store |]
