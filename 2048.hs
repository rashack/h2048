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
  game start

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
