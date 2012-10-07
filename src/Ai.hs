module Ai (
    DoTurn
  , doTurn
  , showBorders
) where

import           Control.Applicative
import           Data.Maybe          (catMaybes)
import qualified Data.Set            as S

import           Control.Monad       (join)
import           Protocol
import           Search
import           Util
import           World

type DoTurn = GameState -> (GameState, [Order])

antCheckDist = 8

doTurn :: DoTurn
doTurn gs = (ngs, orders)
  where ngs = updateMystery gs
        orders = catMaybes generatedOrders
        generatedOrders = generateOrders (world ngs) <$> myAnts (gameAnts ngs)

generateOrders :: World -> Ant -> Maybe Order
generateOrders w a = (,) a <$> bfsMysteriousDir w antCheckDist (antTile w a)

collectFoods :: World -> Foods -> Ants -> Orders
collectFoods w foods ants = moveToOrder <$> bfsMovesTo w 10 foods ants []

updateMystery :: GameState -> GameState
updateMystery gs = gs { world = exploration <$> w }
  where w = world gs
        allReachableTiles = bfsTilesFroms w antCheckDist (antTile w <$> gameAnts gs)
        increment t = t { mystery = mystery t + 1 }
        reachable = flip S.member allReachableTiles
        exploration t = if reachable t then t {mystery = 0} else increment t

showBorders :: GameState -> Direction -> String -> [String]
showBorders gs dir color = fillColor color : showTiles
  where w = world gs
        antTiles = antTile w <$> myAnts (gameAnts gs)
        borderTiles = join $ bfsBorders w antCheckDist dir <$> antTiles
        showTiles = showTile <$> borderTiles
        showTile = drawPoint . point
