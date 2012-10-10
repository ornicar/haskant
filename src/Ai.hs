module Ai (
    DoTurn
  , doTurn
) where

import           Control.Applicative
import           Data.List           ((\\))
import qualified Data.Map            as M
import           Data.Maybe          (catMaybes)
import qualified Data.Set            as S

import           Control.Monad       (join)
import           Protocol
import           Search
import           Tore
import           Util
import           World

type DoTurn = GameState -> IO (GameState, [Order])

exploreDist = 7
foodDist = 8

doTurn :: DoTurn
doTurn gs = return (ngs, preventCollisions (world ngs) allOrders)
  where ngs = updateMystery gs
        w = world ngs
        allOrders = foodOrders ++ exploreOrders
        antPoints = fst <$> myAnts (gameAnts ngs)
        foodTargets = collectFoods w ((w %!) <$> gameFoods ngs) antPoints
        foodOrders = (moveToOrder . fst) <$> foodTargets
        exploreAnts = antPoints \\ (fst <$> foodOrders)
        exploreOrders = explore w exploreAnts

explore :: World -> [Point] -> [Order]
explore w ants = catMaybes $ exploreAnt <$> ants
  where exploreAnt a = (,) a <$> bfsMysteriousDir w exploreDist (w %! a)

collectFoods :: World -> [Tile] -> [Point] -> [(Move, Point)]
collectFoods w = bfsMovesTo w foodDist

preventCollisions :: World -> [Order] -> [Order]
preventCollisions w orders = M.elems orderMap
  where orderMap = M.fromList $ (\o -> (uncurry (toreMove w) o, o)) <$> orders

updateMystery :: GameState -> GameState
updateMystery gs = gs { world = exploration <$> w }
  where w = world gs
        allReachableTiles = bfsTilesFroms w exploreDist (antTile w <$> myAnts (gameAnts gs))
        increment t = t { mystery = mystery t + 1 }
        reachable = flip S.member allReachableTiles
        exploration t = if reachable t then t {mystery = 0} else increment t

_showTargets :: [(Move, Point)] -> String -> [String]
_showTargets targets color = lineColor color : (targets >>= showTarget)
  where showTarget ((a, b), c) = drawArrow a <$> [b, c]

_showBorders :: GameState -> Direction -> String -> [String]
_showBorders gs dir color = fillColor color : showTiles
  where w = world gs
        antTiles = antTile w <$> myAnts (gameAnts gs)
        borderTiles = join $ bfsBorders w exploreDist dir <$> antTiles
        showTiles = showTile <$> borderTiles
        showTile = drawPoint . point
