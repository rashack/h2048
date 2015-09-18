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
  game g r'
  return ()

game :: Grid -> StdGen -> IO Grid
game grid rnd = do
  putStrLn ""
  GridOps.pGrid grid
  ch <- getCh
  case charToDirection ch of
    Just char -> do let (g', rnd') = GridOps.spawn rnd $ GridOps.move grid char
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
