module Main where

import System.IO

data Direction = East | North | West | South

type Grid = [[Int]]

start :: Grid
start = [[0, 0, 0, 0],
         [0, 0, 0, 0],
         [0, 0, 0, 2],
         [0, 0, 0, 2]]

main :: IO ()
main = do
  -- pGrid start
  -- putStrLn ""
  -- pGrid $ move start West
  -- putStrLn ""
  -- pGrid $ transpose start
  -- putStrLn ""
  -- pGrid $ move (transpose start) West
  game start

game grid = do
  putStrLn ""
  pGrid grid
  ch <- getCh
--  direction <- charToDirection ch
--  game $ move grid direction
  game $ move grid (charToDirection ch) -- direction

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

move :: Grid -> Maybe Direction -> Grid
move g Nothing = g
move g (Just North) = transpose $ map merge (transpose g)
move g (Just East)  = colReverse $ map merge (colReverse g)
move g (Just South) = rowReverse $ transpose $ map merge (transpose $ rowReverse g)
move g (Just West)  = map merge g

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
