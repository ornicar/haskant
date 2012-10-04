module Algo where

import           Control.Applicative
import           Data.List
import           Data.Tree

import           Tore
import           World

_breadth :: Tree a -> [a]
_breadth nd =  map rootLabel $ nd : breadth' [nd]
  where
      breadth' [] = []
      breadth' nds = let cs = foldr ((++).subForest) [] nds in
        cs ++ breadth' cs

-- point surroundings as a rose tree
surroundings :: World
  -> Int -- distance
  -> [Point] -- visited
  -> Point -- start point
  -> Tree Point
surroundings w dist visited p = Node p forest
  where neighbors = pointNeighbors w p \\ visited
        newDist = dist - 1
        newVisited = p : visited ++ neighbors
        forest
          | dist == 0 = []
          | null neighbors = []
          | otherwise = surroundings w newDist newVisited <$> neighbors

-- antsFarExploration :: World -> [Ant] -> [Maybe Tile]
