module Main where

import Control.Monad.State
import Grid
import System.IO
import System.Random as Rnd
import qualified GridOps

main :: IO ()
main = do
  r <- Rnd.getStdGen
  let (g, r') = GridOps.startGrid r
  GridOps.pGrid g
  game g r'
  return ()

game :: Grid -> StdGen -> IO Grid
game grid rnd = do
  ch <- getCh
  case charToDirection ch of
    Just char -> do (grid', rnd') <- maybeMove grid char rnd
                    game grid' rnd'
    Nothing   -> game grid rnd

maybeMove :: Grid -> Direction -> StdGen -> IO (Grid, StdGen)
maybeMove g c rnd = if g == g'
                    then return (g, rnd)
                    else do putStrLn ""
                            GridOps.pGrid g''
                            return (g'', rnd')
  where g' = GridOps.move g c
        (g'', rnd') = GridOps.spawn rnd g'


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
