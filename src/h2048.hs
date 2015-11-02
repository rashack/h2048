module Main where

import Control.Monad.State
import Grid
import System.Random
import UI.HSCurses.Curses
import qualified GridOps

main :: IO ()
main = do
  initCurses
  keypad stdScr True -- make the cursor keys usable
  echo False
  cursSet CursorInvisible
  r <- getStdGen
  let (g, r') = runState GridOps.startGrid r
  ppGrid g
  game g r'
  endWin

game :: Grid -> StdGen -> IO Grid
game grid rnd = do
  ch <- getCh
  case ch of
    KeyLeft  -> tryMove grid West rnd
    KeyDown  -> tryMove grid South rnd
    KeyRight -> tryMove grid East rnd
    KeyUp    -> tryMove grid North rnd
    KeyChar 'q' -> return grid
    _           -> game grid rnd

tryMove :: Grid -> Direction -> StdGen -> IO Grid
tryMove grid dir rnd = do (grid', rnd') <- maybeMove grid dir rnd
                          game grid' rnd'

maybeMove :: Grid -> Direction -> StdGen -> IO (Grid, StdGen)
maybeMove g c rnd = if g == g'
                    then return (g, rnd)
                    else do putStrLn ""
                            ppGrid g''
                            return (g'', rnd')
  where g' = GridOps.move g c
        (g'', rnd') = runState (GridOps.spawn g') rnd

ppGrid :: Grid -> IO ()
ppGrid g = do
  erase
  mvWAddStr stdScr 0 0 (show (maximum $ map length g) ++ " " ++ show (length g))
  mvPutGrid 4 4 g
  refresh

mvPutRow :: Int -> Int -> Int -> [Int] -> IO ()
mvPutRow y x w row = mvWAddStr stdScr y x $ concatMap (xpnd w) row

mvPutGrid :: Int -> Int -> [[Int]] -> IO ()
mvPutGrid y x g = do
  mapM_ (\(y', r) -> mvPutRow y' x w r) $ zip [y..] g
  return ()
    where w = 1 + length (show (maximum $ map maximum g))

xpnd :: Int -> Int -> String
xpnd w i = replicate (w - l) ' ' ++ s
  where s = show i
        l = length s
