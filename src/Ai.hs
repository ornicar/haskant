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
doTurn gs = mapMaybe (tryOrder (world explored)) generatedOrders
  where explored = updateExplore gs
        generatedOrders = map generateOrders $ myAnts $ ants explored

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

updateExplore :: GameState -> GameState
updateExplore gs = gs { world = exploration <$> world gs }
  where increment t = t { explore = explore t + 1 }
        antClose t = any (closeBy ((toreBound . world) gs) (point t) . antPoint) $ ants gs
        exploration t = if antClose t then t {explore = 0} else increment t
