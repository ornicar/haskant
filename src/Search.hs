module Search(
  showSurrounding
) where

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
surroundings :: Int -- distance
  -> [Point] -- visited
  -> World
  -> Point -- start point
  -> Tree Point
surroundings dist visited w p = Node p forest
  where neighbors = pointNeighbors w p \\ visited
        newDist = dist - 1
        newVisited = p : visited ++ neighbors
        forest
          | dist == 0 = []
          | null neighbors = []
          | otherwise = surroundings newDist newVisited w <$> neighbors

closeSurroundings :: World -> Point -> Tree Point
closeSurroundings = surroundings 2 []

showSurrounding :: World -> String
showSurrounding w = drawTree strTree
  where strTree = show <$> tree
        tree = closeSurroundings w (10, 10)

antsFarExploration :: World -> [Ant] -> [Maybe Tile]
antsFarExploration = undefined
