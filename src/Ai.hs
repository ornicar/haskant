module Ai (
    doTurn
  , orderPassable
) where

import Control.Applicative
import           Data.List
import           Data.Maybe          (mapMaybe)

import           Protocol
import           Tore
import           World

-- Entry point
doTurn :: GameState -> [Order]
doTurn gs = mapMaybe (tryOrder (world mysterious)) generatedOrders
  where mysterious = updateMystery gs
        generatedOrders = map generateOrders $ myAnts $ ants mysterious

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
updateMystery gs = gs { world = exploration <$> world gs }
  where increment t = t { mystery = mystery t + 1 }
        antClose t = any (closeBy ((toreBound . world) gs) (point t) . antPoint) $ ants gs
        exploration t = if antClose t then t {mystery = 0} else increment t
