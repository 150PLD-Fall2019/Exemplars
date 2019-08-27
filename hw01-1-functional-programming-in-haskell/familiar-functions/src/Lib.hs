module Lib 
(head, tail, filter, map, foldr, fst, snd, (++), (+?), Logger, lift, app, lower)
where

import Prelude hiding (head, tail, fst, snd, filter, map, foldr, (++))


-- this is a single line comment 

{- this is a block
 - or multiline comment
 -}

head :: [a] -> a -- type declaration. the "forall a ." is implied.
head = undefined 

tail :: [a] -> [a] 
tail = undefined 

filter :: (a -> Bool) -> [a] -> [a]
filter = undefined 

map :: (a -> b) -> [a] -> [b] 
map = undefined 

foldr :: (a -> b -> b) -> b -> [a] -> b 
foldr = undefined 

fst :: (a,b) -> a
fst = undefined 

snd :: (a,b) -> b 
snd = undefined 

(++) :: [a] -> [a] -> [a]
(++) = undefined 

(+?) :: Maybe Int -> Maybe Int -> Maybe Int 
(+?) = undefined 

data Logger a 

lift :: String -> a -> Logger a
lift = undefined 

app :: Logger (a -> b) -> Logger a -> Logger b
app = undefined

lower :: Logger a -> (String, a)
lower = undefined


