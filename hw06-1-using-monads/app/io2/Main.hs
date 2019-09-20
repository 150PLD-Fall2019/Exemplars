module Main where
import System.Random

{-
 -
 - run as an executable: >stack exec io2
 - This exercise asks you to use the I/O Monad to implement a simple
 - number guessing game by filling in the definitions of a couple of
 - functions in this file.  In this game, the program selects a random
 - integer in the set {1 . . . 10} that the player has to guess. The
 - player guesses by entering a number at the prompt. If the player does
 - not guess correctly, the program tells the player if the guess was too
 - high or low, and asks for another guess. Otherwise, the program prints
 - a congratulatory message and exits.  
 -
 - Here is an example session:
 -
 - > stack exec io2
 - I'm thinking of a number between 1 and 10.  Can you guess it?
 - 5
 - Too low!
 - 8
 - Too low!
 - 9
 - Too low!
 - 10
 - Congratulations!
 -
 -
 - (a) The game asks the player to guess until the player guesses
 - correctly. To implement this behavior, it is useful to have a control
 - structure that executes an action until some condition is true (in our
 - case, the player guessing correctly.)  Fill in the code for untilIO 
 - below. The function untilIO takes an IO action producing a Bool
 - and returns an IO action producing nothing: 
 -
 -   untilIO :: IO Bool -> IO ().
 -
 -   The function untilIO should first execute the loop body (the
 -   argument) and then check the loop condition (the Bool produced by the
 -   argument). If the loop condition returns True, untilIO should exit.
 -   -}


untilIO :: IO Bool -> IO ()
untilIO action = undefined -- FILL IN CODE HERE


-- {-
-- (b) Now implement the core of the game, 
--     doGuess :: Int -> IO Bool, 
--     which takes the randomly-generated secret number as an argument, gets
--     the player’s current guess using an IO action, prints whether the
--     guess was low (Too low!), high (Too high!), or just right
--     (Congratulations!), and returns a Bool indicating whether the player
--     finished the game.  -}
--
--     -- Some useful functions:
--     --   getLine :: IO String              -- Reads a line of input
--     --   read :: String -> Int             -- converts a string of digits into an Int
--     --   (Or more generally:
--     --   read :: Read t => String -> t     -- Parse any type t belonging to Read class to a value of type t)
--     --   putStrLn :: String -> IO ()       -- Print string to output
--     --   compare :: (Ord a) => a -> a -> Ordering 
--     --   return  :: (Monad m) => a -> m a  -- lifts pure values into monadic ones
doGuess :: Int -> IO Bool
doGuess secret = undefined --     -- FILL IN CODE HERE


main :: IO ()
main = do
{  secret <- randomRIO(1,10) -- http://hackage.haskell.org/package/random-1.1/docs/System-Random.html
;  putStrLn "I'm thinking of a number between 1 and 10.  Can you guess it?"
;  untilIO (doGuess secret)
;  putStrLn "Congratulations!"
}
