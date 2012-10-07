module Ai (
    DoTurn
  , doTurn
  , orderPassable
) where

import           Control.Applicative
import           Data.List
import           Data.Maybe          (mapMaybe)
import qualified Data.Set as S

import           Protocol
import           Search
import           Tore
import           World

type DoTurn = GameState -> (GameState, [Order])

-- Entry point
doTurn :: DoTurn
doTurn gs = (ngs, orders)
  where orders = mapMaybe (tryOrder (world ngs)) generatedOrders
        ngs = updateMystery gs
        generatedOrders = map generateOrders $ myAnts $ ants ngs

-- | Picks the first "passable" order in a list
-- returns Nothing if no such order exists
tryOrder :: World -> [Order] -> Maybe Order
tryOrder w = find (orderPassable w)

-- | Generates orders for an Ant in all directions
generateOrders :: Ant -> [Order]
generateOrders a = [(a,d) | d <- [North .. West]]

orderPassable :: World -> Order -> Bool
orderPassable w (ant, dir) = content (w %! newPoint) /= Water
  where newPoint = move dir (antPoint ant)

updateMystery :: GameState -> GameState
updateMystery gs = gs { world = exploration <$> w }
  where w = world gs
        allReachableTiles = bfsTilesFroms w 10 (antTile w <$> ants gs) 
        increment t = t { mystery = mystery t + 1 }
        reachable = flip S.member allReachableTiles 
        exploration t = if reachable t then t {mystery = 0} else increment t
