module Main where
import System.Environment -- http://hackage.haskell.org/package/base-4.12.0.0/docs/System-Environment.html
import Data.List -- http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-List.html

--  Write a main program that takes as a command line argument the name of a file (e.g., myfile), reads the file,
--  --  sorts the lines of the file alphabetically, and then writes the result to a file 'myfile.result'.
--
--  -- > stack exec io1 myfile
--  -- > myfile:
--  -- Haskell
--  -- ML
--  -- Miranda
--  -- Java
--  -- C++
--  -- Javascript
--  -- Ruby
--  -- Python
--  -- Perl
--
--  -- > myfile.result:
--  -- C++
--  -- Haskell
--  -- Java
--  -- Javascript
--  -- ML
--  -- Miranda
--  -- Perl
--  -- Python
--  -- Ruby
--
--
--  -- The following functions might be helpful:
--  --  getArgs :: IO [String]    
--  --     When performed, get command-line arguments supplied when invoking compiled program
--  --  readFile :: FilePath -> IO String     
--  --     When performing readFile f, get contents of file f as a string
--  --     Type FilePath is synonymous with String
--  --  writeFile :: FilePath -> String -> IO ()
--  --     When performing writeFile f s, writes the value s to the file f.
--  --  lines :: String -> [String]
--  --     lines breaks a string up into a list of strings at newline characters. The resulting strings do not contain newlines.
--  --  unlines :: [String] -> String	
--  --     unlines is an inverse operation to lines. It joins lines, after appending a terminating newline to each.
--  --  compare :: String -> String -> Ordering
--  --     compare indicates whether the first argument is LT, EQ, or GT the second
--  --  sortBy :: (a -> a -> Ordering) -> [a] -> [a]
--  --     takes a comparison function and a list and returns a new list sorted by the supplied comparison function.
--
main :: IO () 
main = undefined
