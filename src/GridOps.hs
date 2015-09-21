module GridOps where

import Control.Monad.State
import Grid
import System.Random

type Pos = (Int, Int)

size = 4

-- This might be 2 or 4 in the future.
spawnInt = 2

cleanGrid :: Grid
cleanGrid = replicate size $ replicate size 0

startGrid :: State StdGen Grid
startGrid = spawn cleanGrid >>= spawn

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
nextInt = liftM next get >>= \(n, g) -> put g >> return n

nextPos :: State StdGen (Int, Int)
nextPos = do
  x <- nextInt
  y <- nextInt
  return (x `mod` size, y `mod` size)

-- spawn a new number at an "open" position
spawn :: Grid -> State StdGen Grid
spawn g = do
  pos <- nextPos
  if spawnable pos g
          then return (setg spawnInt pos g)
          else spawn g

spawnable :: Pos -> Grid -> Bool
spawnable pos g = getg pos g == 0

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
