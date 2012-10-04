module Algo where

import           Data.Tree

import           Ants

_breadth :: Tree a -> [a]
_breadth nd =  map rootLabel $ nd : breadth' [nd]
  where
      breadth' [] = []
      breadth' nds = let cs = foldr ((++).subForest) [] nds in
        cs ++ breadth' cs

-- tile surroundings as a rose tree
tileSurroundings :: World -> Ant -> Tree Tile
-- antsFarExploration :: World -> [Ant] -> [Maybe Tile]
