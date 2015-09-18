module Main where

import Control.Monad.State
import System.IO
import System.Random as Rnd
import Debug.Trace

data Direction = East | North | West | South

type Grid = [[Int]]

size = 4

-- This might be 2 or 4 in the future.
spawnInt = 2

cleanGrid :: Grid
cleanGrid = replicate size $ replicate size 0

start :: StdGen -> (Grid, StdGen)
start r = (g'', r'')
  --trace ("foo " ++ show x) (setg spawnInt (x, y) $ setg spawnInt (y, x) cleanGrid)
  where
    (g', r')   = spawn r cleanGrid
    (g'', r'') = spawn r' g'

getg :: (Int, Int) -> Grid -> Int
getg (x, y) g = (g !! y) !! x

setg :: Int -> (Int, Int) -> Grid -> Grid
setg e (x, y) g =
  setl row' y g
  where row  = g !! y
        row' = (setl e x row)

setl :: a -> Int -> [a] -> [a]
setl _ _ []    = []
setl e 0 (_:t) = e:t
setl e n (h:t) = h : setl e (n-1) t

rand :: StdGen -> Int -> (Int, StdGen)
rand r m = (n `mod` m, r')
  where (n, r') = next r

randPos :: StdGen -> ((Int, Int), StdGen)
randPos r = ((x, y), r'')
  where (x, r')  = rand r  size
        (y, r'') = rand r' size

-- spawn a new number at an "open" position
spawn :: StdGen -> Grid -> (Grid, StdGen)
spawn r g = if spawnable pos g
            then (setg spawnInt pos g, r')
            else spawn r' g
  where (pos, r') = randPos r

spawnable :: (Int, Int) -> Grid -> Bool
spawnable pos g = getg pos g == 0

main :: IO ()
main = do
  r <- Rnd.getStdGen
  let (g, r') = start r
  game g r'
  return ()

game :: Grid -> StdGen -> IO Grid
game grid rnd = do
  putStrLn ""
  pGrid grid
  ch <- getCh
  case charToDirection ch of
    Just char -> do let (g', rnd') = spawn rnd $ move grid char
                    game g' rnd'
    Nothing   -> game grid rnd

getCh :: IO Char
getCh = do hSetEcho stdin False
           buffering <- hGetBuffering stdin
           hSetBuffering stdin NoBuffering
           c <- getChar
           hSetEcho stdin True
           hSetBuffering stdin buffering
           return c

charToDirection :: Char -> Maybe Direction
charToDirection ch =
  case ch of
    'a' -> Just West
    's' -> Just South
    'd' -> Just East
    'w' -> Just North
    _   -> Nothing

move :: Grid -> Direction -> Grid
move g North = transpose $ map merge (transpose g)
move g East  = colReverse $ map merge (colReverse g)
move g South = rowReverse $ transpose $ map merge (transpose $ rowReverse g)
move g West  = map merge g

merge :: [Int] -> [Int]
merge xs = merged ++ padding
  where
    merged = combine $ filter (/= 0) xs
    padding = replicate (length xs - length merged) 0
    combine (x:y:xs) | x == y = 2 * x : combine xs
                     | otherwise =  x : combine (y:xs)
    combine x        = x

transpose :: Grid -> Grid
transpose ([]:_) = []
transpose g = (map head g) : transpose (map tail g)

colReverse :: Grid -> Grid
colReverse = map reverse

rowReverse :: Grid -> Grid
rowReverse = reverse

pGrid (r:[]) = print r
pGrid (r:rs) = do
  print r
  pGrid rs
