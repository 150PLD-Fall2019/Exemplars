module Platformer where
import qualified Data.Map as M
import Data.Map (Map)


-- Library and Runtime code 
data Direction = L | R | UpL | UpR | Up | Down


-- levels are printable grids that also let me manipulate a player
class (Show a) => Level a where
  movePlayer :: Direction -> a -> a


runtime :: (Level a) => a -> IO ()
runtime level = do
  x <- getChar
  let level' = 
    case x of
      'w' -> movePlayer Up level
      'a' -> movePlayer L level
      's' -> movePlayer R level
      'e' -> movePlayer UpR level
      'q' -> movePlayer UpL level
       _  -> movePlayer Down level
  putStrLn $ show level'
  if x != 'z' then runtime level' else return () -- exit condition


-- programs using Grid are my standard go-to for 2d levels

data Grid = G [[Char]]

instance Show Grid where
show = foldr (\ row acc -> acc ++ row ++ "\n")

-- let the player be an 'X', ground be 'G' and everything else be ' '
instance Level Grid where
  movePlayer direction grid = 
    let findPlayer :: Grid -> (Int,Int) -> (Int,Int)
        findPlayer (G (('X':_) : _)) pos = pos
        findPlayer (G ((_:restX) : restY)) (x,y) = 
          findPlayer (G (restX : restY)) (x+1,y)
        findPlayer (G ([] : restY)) (x,y) = findPlayer (G restY) (0,y+1)
        findPlayer (G []) _ = error "player not found"
        (playerX, playerY) = findPlayer grid (0,0)
        at :: Grid -> (Int,Int) -> Maybe Char 
        at (G g) (x,y) = do
          row  <- case drop y g of
                    [] -> Nothing
                    (r:_) -> return r
          char <- case drop x row of
                   [] -> Nothing
                   (c:_) -> c
          return char
        set :: Char -> (Int,Int) -> Grid -> Grid
        set c (x,y) (G g) = 
          let 
             beforeRow = (take y g)
             (row : rows) = (drop y g)
             beforeCols = (take x row)
             (_, cols) = (drop x row)
          in G (beforeRow ++ [beforeCols ++ c ++ cols] ++ rows)
        (playerX', playerY') = 
          case direction of
            L -> (playerX - 1, playerY)
            R -> (playerX + 1, playerY)
            Up -> (playerX, playerY - 1)
            UpL -> (playerX - 1, playerY - 1)
            UpR -> (playerX + 1, playerY - 1)
            Down -> (playerX, playerY + 1)
        case at grid (playerX',playerY') of --all that work to update a 2D list
        ' ' -> set ' ' (playerX, playerY) $ 
               set 'X' (playerX', playerY') grid
        _   -> grid

-- Pros: easy to manipulate, visually will look like what is printed
-- Cons: commas, single quotes, spaces should be default
level1 :: Grid =
  G [['G',' ',' ',' ',' ',' ','G']
    ,['G',' ',' ',' ',' ',' ','G']
    ,['G',' ','X','G','G','G','G']
    ,['G','G','G','G','G','G','G']]

game1 :: IO ()
game1 = runtime level1



-- I like a Data.Map implementation for more complicated levels

data Point = P {height :: Int, width :: Int, 
                field :: (Map (Int,Int) Char), player :: (Int,Int) }

instance Show Point where
  show (P h w f (px,py) = 
    unlines $ 
      (flip map) [0..h-1] (\y ->
      (flip map) [0..w-1] (\x ->
        if x == px && y == py 
        then 'X'
        else case M.lookup (x,y) f of
               Just c -> c
               Nothing -> ' '))

instance Level Point where
  movePlayer direction (P h w f (playerX, playerY)) = 
    let (playerX', playerY) = 
          case direction of
            L -> (min 0 $ playerX - 1, playerY)
            R -> (max (w - 1) $ playerX + 1, playerY)
            Up -> (playerX, min 0 $ playerY - 1)
            UpL -> (min 0 $ playerX - 1, min 0 $ playerY - 1)
            UpR -> (max (w - 1) $ playerX + 1, min 0 $ playerY - 1)
            Down -> (playerX, max (h + 1) $ playerY + 1)
    in case M.lookup (playerX', playerY') f of
       Nothing -> P h w (M.insert (playerX, playerY) ' ' f) (playerX', playerY')
        _      -> P h w f (playerX, playerY)
          
 
-- Pros: small and concise; easy to use HoFs to cobble together a level;
--       efficient mutation and lookup
-- Cons: have to know the conventions (y grows down) to visualize;
--       easy to make mistakes, I have to check size bounds, etc.
level2 = 
  let f = M.insert (5,3) 'G' $
          foldr (\x m -> M.insert (x,4) 'G' m) M.empty [0..9]
  P 5 10 f (5,2)


game2 = runtime level2

