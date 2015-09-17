module Main where

import Control.Monad.State
import System.IO
import System.Random as Rnd
import Debug.Trace

data Direction = East | North | West | South

type Grid = [[Int]]

cleanGrid :: Grid
cleanGrid = replicate 4 $ replicate 4 0

start :: StdGen -> Grid
start r = do
  trace ("foo " ++ show x) (set 2 (x, y) $ set 2 (y, x) cleanGrid)
  -- set (x, y) cleanGrid
  --   where rnd = Rnd.getStdGen
    where (x', r') = next r
          (y', _)  = next r'
          x = x' `mod` 4
          y = y' `mod` 4

set :: Int -> (Int, Int) -> Grid -> Grid
set e (x, y) g =
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

-- spawn :: StdGen -> Grid -> Grid
-- spawn r g = ...
--     where (x', r') = next r
--           (y', _)  = next r'
--           x = x' `mod` 4
--           y = y' `mod` 4

main :: IO ()
main = do
  r <- Rnd.getStdGen
  game $ start r
  return ()

game :: Grid -> IO Grid
game grid = do
  putStrLn ""
  pGrid grid
  ch <- getCh
  case charToDirection ch of
    Just char -> game $ move grid char
    Nothing   -> game grid

getCh :: IO Char
getCh = do hSetEcho stdin False
           c <- getChar
           hSetEcho stdin True
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
