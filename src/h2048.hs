module Main where

import Brick
import qualified Brick.AttrMap as A
import qualified Brick.Main as BM
import qualified Brick.Types as BT
import qualified Brick.Widgets.List as L
import Control.Monad.State
import Grid as G
import System.Random
import qualified Graphics.Vty as V
import qualified GridOps

ui :: (Grid, StdGen) -> [Widget]
ui (g, _r) = [str $ grid2str g]

appEvent :: (Grid, StdGen) -> V.Event -> BT.EventM (BT.Next (Grid, StdGen))
appEvent (g, r) e =
  case e of
    V.EvKey V.KEsc []   -> BM.halt (g, r)
    V.EvKey V.KRight [] -> BM.continue $ maybeMove g East r
    V.EvKey V.KLeft []  -> BM.continue $ maybeMove g West r
    V.EvKey V.KUp []    -> BM.continue $ maybeMove g North r
    V.EvKey V.KDown []  -> BM.continue $ maybeMove g South r
    _ev                 -> BM.continue (g, r)

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (L.listAttr,            V.white `on` V.blue)
    , (L.listSelectedAttr,    V.blue `on` V.white)
    ]

app :: BM.App (Grid, StdGen) V.Event
app =
  BM.App { BM.appDraw = ui
         , BM.appChooseCursor = BM.showFirstCursor
         , BM.appHandleEvent = appEvent
         , BM.appStartEvent = return
         , BM.appAttrMap = const theMap
         , BM.appLiftVtyEvent = id
         }

main :: IO ()
main = do
  r <- getStdGen
  let (g, r') = runState GridOps.startGrid r
  void $ BM.defaultMain app (g, r')

maybeMove :: Grid -> G.Direction -> StdGen -> (Grid, StdGen)
maybeMove g c rnd = if g == g'
                    then (g, rnd)
                    else (g'', rnd')
  where g' = GridOps.move g c
        (g'', rnd') = runState (GridOps.spawn g') rnd

grid2str :: Grid -> String
grid2str g = grid2str2 width g
  where width = 1 + length (show $ maximum $ map maximum g)

grid2str2 :: Int -> Grid -> String
grid2str2 width grid = foldl (++) [] $ lineBreaks $ map (row2str width) grid

row2str :: Int -> [Int] -> String
row2str width row =
  foldl (++) [] $ map (padNum width) row

padNum :: Int -> Int -> String
padNum width num = padding ++ numStr
  where padding = foldl (++) [] $ replicate (width - length numStr) " "
        numStr = show num

lineBreaks :: [String] -> [String]
lineBreaks []       = []
lineBreaks (x:y:[]) = [x] ++ ["\n"] ++ [y]
lineBreaks (x:xs)   = [x] ++ ["\n"] ++ lineBreaks xs
