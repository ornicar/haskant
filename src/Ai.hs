module Ai (
    DoTurn
  , doTurn
) where

import           Control.Applicative
import           Data.Maybe          (catMaybes)
import qualified Data.Set            as S

import           Control.Monad       (join)
import           Protocol
import           Search
import           Util
import           World
import           Tore

type DoTurn = GameState -> IO (GameState, [Order])

antCheckDist = 8

doTurn :: DoTurn
doTurn gs = do
  mapM_ putStrLn $ showTargets foodTargets black
  return (ngs, exploreOrders)
  where ngs = updateMystery gs
        w = world ngs
        allAnts = myAnts (gameAnts ngs)
        foodTargets = _debugPrefix "food targets" $ collectFoods w ((w %!) <$> gameFoods ngs) allAnts
        foodOrders = (moveToOrder . fst) <$> foodTargets
        exploreAnts = allAnts -- \\ (fst <$> foodOrders)
        exploreOrders = explore w exploreAnts

explore :: World -> Ants -> [Order]
explore w ants = catMaybes $ exploreAnt <$> ants
  where exploreAnt a = (,) a <$> bfsAttractiveDir w antCheckDist (antTile w a)

collectFoods :: World -> [Tile] -> Ants -> [(Move, Point)]
collectFoods w = bfsMovesTo w 10 

updateMystery :: GameState -> GameState
updateMystery gs = gs { world = exploration <$> w }
  where w = world gs
        allReachableTiles = bfsTilesFroms w antCheckDist (antTile w <$> gameAnts gs)
        increment t = t { mystery = mystery t + 1 }
        reachable = flip S.member allReachableTiles
        exploration t = if reachable t then t {mystery = 0} else increment t

showTargets :: [(Move, Point)] -> String -> [String]
showTargets targets color = lineColor color : (targets >>= showTarget)
  where showTarget ((a, b), c) = drawArrow a <$> [b, c]

showBorders :: GameState -> Direction -> String -> [String]
showBorders gs dir color = fillColor color : showTiles
  where w = world gs
        antTiles = antTile w <$> myAnts (gameAnts gs)
        borderTiles = join $ bfsBorders w antCheckDist dir <$> antTiles
        showTiles = showTile <$> borderTiles
        showTile = drawPoint . point
