module Main where

import Control.Monad.State
import System.IO
import System.Random
import Debug.Trace

data Direction = East | North | West | South

type Grid = [[Int]]

type Pos = (Int, Int)

size :: Int
size = 4

-- This might be 2 or 4 in the future.
spawnInt :: Int
spawnInt = 2

cleanGrid :: Grid
cleanGrid = replicate size $ replicate size 0

start :: State StdGen Grid
start = spawn cleanGrid >>= spawn

getg :: Pos -> Grid -> Int
getg (x, y) g = (g !! y) !! x

setg :: Int -> Pos -> Grid -> Grid
setg e (x, y) g =
  setl row' y g
  where row  = g !! y
        row' = setl e x row

setl :: a -> Int -> [a] -> [a]
setl _ _ []    = []
setl e 0 (_:t) = e:t
setl e n (h:t) = h : setl e (n-1) t

nextInt :: State StdGen Int
nextInt = do
  g <- get
  let (n, g') = next g
  put g'
  return n

-- spawn a new number at an "open" position
spawn   :: Grid -> State StdGen Grid
spawn g = do
  pos <- randPos
  if spawnable pos g
    then return $ setg spawnInt pos g
    else spawn g
  where randPos = do
          x <- nextInt
          y <- nextInt
          return (x `mod` size, y `mod` size)

spawnable :: Pos -> Grid -> Bool
spawnable pos g = getg pos g == 0

main :: IO ()
main = do
  r <- getStdGen
  let (g, r') = runState start r
  forever $ game g r'

game :: Grid -> StdGen-> IO Grid
game grid gen = do
  putStrLn ""
  mapM_ print grid
  ch <- getCh
  case charToDirection ch of
    Just char -> let (g', gen') = runState (spawn $ move grid char) gen
                 in game g' gen'
    Nothing   -> game grid gen

getCh :: IO Char
getCh = do hSetEcho stdin False
           buffering <- hGetBuffering stdin
           hSetBuffering stdin NoBuffering
           c <- getChar
           hSetEcho stdin True
           hSetBuffering stdin buffering
           return c

commands :: [(Char, Direction)]
commands = [ ('a', West), ('s', South), ('d', East), ('w', North)]

charToDirection :: Char -> Maybe Direction
charToDirection ch = lookup ch commands

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
transpose g = map head g : transpose (map tail g)

colReverse :: Grid -> Grid
colReverse = map reverse

rowReverse :: Grid -> Grid
rowReverse = reverse
