module Main where


-- given:
readMaybe :: (Read a) => String -> Maybe a
getLine :: IO String
(>>=) :: (Monad m) => m a -> (a -> m b) -> m b
return :: (Monad m) => a -> m a

-- suppose: 
eBindBad :: IO (Maybe Int)
eBindBad =
  getLine >>= (\input1 ->
  getLine >>= (\input2 -> 
    (readMaybe input1 >>= (\x ->
     readMaybe input2 >>= (\y ->
     return $ x + y))) >>= 
  (\z ->
    case z of 
    Nothing -> putStrLn "something went wrong" >>= (\_ ->
               return Nothing)
    Just z'  -> putStrLn (show z') >>= (\_ ->
                return (Just z'))))))

-- and it's translation into do notation.
eDoBad :: IO (Maybe Int)
eDoBad = do
  input1 <- getLine
  input2 <- getLine 
  z <- do
    x <- readMaybe input1
    y <- readMaybe input2
    return $ x + y
  case z of
    Nothing -> do
      putStrLn "something went wrong"
      return Nothing
    Just z' -> do
      putStrLn (show z')
      return (Just z')

-- These expressions contain a type error with respect to monadic bind
-- Questions:
--   1) What is the type of `z` on the third line?
--   2) What is the type of `return $ x + y` on the sixth line?
--   3) Describe the type error


-- 4) Rewrite the bind so that it will type check
eRewritten :: IO (Maybe Int)
eRewritten = 

-- 5) Translate it into do notation

eCorrect :: IO (Maybe Int)
eCorrect = do
  input1 <- getLine
  input2 <- getLine
  let z = do
    x <- readMaybe input1
    y <- readMaybe input2
    return $ x + y
  case z of
    Nothing -> do
      putStrLn "something went wrong"
      return Nothing
    Just z' -> do
      putStrLn (show z')
      return (Just z')

eCorrectWithAnnotations :: IO (Maybe Int)
eCorrectWithAnnotations = do
  input1 <- getLine :: IO String
  input2 <- getLine :: IO String
  let z = (do
    x <- (readMaybe input1) :: (Read a) => String -> Maybe a
    y <- (readMaybe input2) :: (Read a) => String -> Maybe a
    return $ x + y) :: (Num a, Read a) => Maybe a
  case z of
    Nothing -> do
      putStrLn "something went wrong" :: IO ()
      return Nothing :: IO (Maybe Int)
    Just z' -> do
      putStrLn (show z') :: IO ()
      return (Just z')   :: IO (Maybe Int)



