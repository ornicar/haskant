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
antExploreDist = antCheckDist + 1

-- Entry point
doTurn :: DoTurn
doTurn gs = (ngs, orders)
  where ngs = updateMystery gs
        orders = catMaybes generatedOrders
        generatedOrders = generateOrders (world ngs) <$> myAnts (ants ngs)

-- | Generates orders for an Ant in all directions
generateOrders :: World -> Ant -> Maybe Order
generateOrders w a = (,) a <$> bfsMysteriousDir w antExploreDist (antTile w a)

updateMystery :: GameState -> GameState
updateMystery gs = gs { world = exploration <$> w }
  where w = world gs
        allReachableTiles = bfsTilesFroms w antCheckDist (antTile w <$> ants gs)
        increment t = t { mystery = mystery t + 1 }
        reachable = flip S.member allReachableTiles
        exploration t = if reachable t then t {mystery = 0} else increment t

showBorders :: GameState -> Direction -> String -> [String]
showBorders gs dir color = fillColor color : showTiles
  where w = world gs
        antTiles = antTile w <$> myAnts (ants gs)
        borderTiles = join $ bfsBorders w antExploreDist dir <$> antTiles
        showTiles = showTile <$> borderTiles
        showTile = drawPoint . point
