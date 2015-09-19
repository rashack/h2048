module Main where

import Control.Monad.State
import Grid
import System.IO
import System.Random as Rnd
import Text.Printf
import qualified GridOps

main :: IO ()
main = do
  r <- Rnd.getStdGen
  let (g, r') = GridOps.startGrid r
  ppGrid g
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
                            ppGrid g''
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

commands :: [(Char, Direction)]
commands = [ ('a', West), ('s', South), ('d', East), ('w', North) ]

charToDirection :: Char -> Maybe Direction
charToDirection ch = lookup ch commands

pGrid :: Grid -> IO ()
pGrid (r:[]) = print r
pGrid (r:rs) = do
  print r
  pGrid rs

ppGrid :: Grid -> IO ()
ppGrid g = ppGrid2 g width
  where width = 1 + length (show $ maximum $ map maximum g)

ppGrid2 :: Grid -> Int -> IO ()
ppGrid2 (r:[]) width = ppRow r width
ppGrid2 (r:rs) width = do
  ppRow r width
  ppGrid2 rs width

ppRow r w = do
  mapM_ (Text.Printf.printf ("%" ++ show w ++ "d")) r
  putStrLn ""
  return ()
