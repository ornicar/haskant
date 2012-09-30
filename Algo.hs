module Algo where

import Data.Graph.Inductive.Query.BFS()
import Data.Graph.Inductive.Tree (Gr)
import Data.Graph.AStar
import Data.Tree

_breadth :: Tree a -> [a]
_breadth nd =  map rootLabel $ nd : breadth' [nd]
  where
      breadth' [] = []
      breadth' nds = let cs = foldr ((++).subForest) [] nds in
        cs ++ breadth' cs
