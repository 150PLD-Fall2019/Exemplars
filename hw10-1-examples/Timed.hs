module Timed where

import Control.Monad (forever)
import Control.Monad.Trans.State
import Data.Map 
import System.IO
import Control.Concurrent
import System.Random

-- here is a stubbed runtime library:
data PinMode = DigitalInput | DigitalOutput | AnalogInput | AnalogOutput 
setPin :: Int -> PinMode -> IO ()
setPin _ _ = return ()

readPin :: Int -> IO Int
readPin 0 = do
  hSetBuffering stdin NoBuffering
  pressed <- hReady stdin
  if pressed then return 1 else return 0
readPin 9 = randomRIO (0,255)
readPin _ = return 0   

writePin :: Int -> Int -> IO ()
writePin _ value = putStrLn $ show value

-- program1 does not need to track any timing, it is simple enough
program1 :: IO ()
program1 = 
  let setup ::  IO () 
      setup = do -- initialize my hardware
        setPin 0 DigitalInput
        setPin 1 AnalogOutput
      loop ::  IO ()
      loop  = do -- I want the button to drive the led as fast as possible
        buttonState <- readPin 0
        writePin 1 (if buttonState == 1 then 255 else 0)
        delayThread (1 * 1000 * 1000) -- 1 second in microseconds as an int
        loop
  in setup >> forever loop

-- program2 gets rid of button jitter by taking in more button inputs,
-- but still only outputting once per second
program2 :: IO ()
program2 = 
  let setup :: IO ()
      setup = do -- initialize my hardware
        setPin 0 DigitalInput
        setPin 1 AnalogOutput
      loop :: IO ()
      loop = do
        buttonState1 <- readPin 0
        delayThread (1 * 1000 * 1000 `div` 10) -- 10 button presses per write
        buttonState2 <- readPin 0
        delayThread (1 * 1000 * 1000 `div` 10) 
        buttonState3 <- readPin 0
        delayThread (1 * 1000 * 1000 `div` 10) 
        buttonState4 <- readPin 0
        delayThread (1 * 1000 * 1000 `div` 10) 
        buttonState5 <- readPin 0
        delayThread (1 * 1000 * 1000 `div` 10) 
        buttonState6 <- readPin 0
        delayThread (1 * 1000 * 1000 `div` 10) 
        buttonState7 <- readPin 0
        delayThread (1 * 1000 * 1000 `div` 10) 
        buttonState8 <- readPin 0
        delayThread (1 * 1000 * 1000 `div` 10) 
        buttonState9 <- readPin 0
        delayThread (1 * 1000 * 1000 `div` 10) 
        buttonState10 <- readPin 0
        delayThread (1 * 1000 * 1000 `div` 10) 
        if and $ map (== 1) 
               [ buttonState1, buttonState2, buttonState3, buttonState4
               , buttonState5, buttonState6, buttonState7, buttonState8
               , buttonState9, buttonState10]
        then writePin 1 255
        else writePin 1 0
  in  setup >> forever loop
-- program3 uses the button to set on/off, and the dial to set brightness
-- the dial hardware is slower than the button
-- the programmer also tried to write nicer code, but this puts the Delays
-- behind an abstraction barrier
program3 :: IO ()
program3 = 
   let setup :: IO ()
       setup = do -- initialize my hardware
        setPin 0 DigitalInput
        setPin 1 AnalogOutput
        setPin 9 AnalogInput
       loop :: IO ()
       loop = do {
         [(fiveButtonStates,dialState1),
         (fiveMoreButtonStates,dialState2)] <-
           (flip mapM) [1..2] (\_ -> 
             do --for every 10 input delays
               fiveButtonStates <- 
                 (flip mapM) [1..5] (\_ -> 
                   do -- for every 5 button presses
                     buttonState <- readPin 0
                     threadDelay (1 * 1000 * 1000 `div` 10)
                     return buttonState
                 )
               dialState <- readPin 9        -- get a dial reading
               return (fiveButtonStates, dialState))
         ;  if and $ map (== 1) (fiveButtonStates ++ fiveMoreButtonStates)
            then writePin 1 $ (dialState1 + dialState2) `div` 2
            else writePin 1 0
         }
  in  setup >> forever loop
