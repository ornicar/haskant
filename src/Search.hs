module Search(
  exploreDirection
) where

import           Control.Applicative
import           Control.Monad.State
import           Data.List
import           Data.Tree
import           GHC.Exts            (sortWith)

import           Data.Maybe          (listToMaybe)
import           Tore
import           World

_breadth :: Tree a -> [a]
_breadth nd =  map rootLabel $ nd : breadth' [nd]
  where
      breadth' [] = []
      breadth' nds = let cs = foldr ((++).subForest) [] nds in
        cs ++ breadth' cs

-- point surroundings as a rose tree
_surroundings :: Int -- distance
  -> [Point] -- visited
  -> World
  -> Point -- start point
  -> Tree Point
_surroundings dist visited w p = Node p forest
  where neighbors = pointNeighbors w p \\ visited
        newDist = dist - 1
        newVisited = p : visited ++ neighbors
        forest
          | dist == 0 = []
          | null neighbors = []
          | otherwise = _surroundings newDist newVisited w <$> neighbors

_closeSurroundings :: World -> Point -> Tree Point
_closeSurroundings = _surroundings 2 []

_showSurrounding :: World -> String
_showSurrounding w = drawTree strTree
  where strTree = show <$> tree
        tree = _closeSurroundings w (10, 10)

exploreDirection :: World -> [Ant] -> [Maybe Direction]
exploreDirection w ants = pickBestDir <$> antsDirScores w ants
  where pickBestDir ds = fst <$> listToMaybe (sortWith (negate . snd) ds)

type DirScore = (Direction, Int)
type DirScores = [DirScore]

antsDirScores :: World -> [Ant] -> [DirScores]
antsDirScores w ants = evaluateAnt <$> ants
  where evaluateAnt ant = evaluate ant <$> directions
        evaluate ant dir = (dir, 0)

type Position = Point
type Distance = Int
type Visited = [Point]

type ExploreOneState = (World, Position, Distance, Visited)

exploreNeighborsOne :: State ExploreOneState [Point]
exploreNeighborsOne = state fun 
  where fun s @ (_, _, 0, _) = ([], s)
        fun (w, p, dist, visited) = (neighbors, newState)
          where neighbors = pointNeighbors w p \\ visited
                newState = (w, p, dist - 1, p : visited ++ neighbors)

type ExploreManyState = (World, [Position], Distance, Visited)

type Exploring = (

exploreNeighborsMany :: State ExploreManyState [[Point]]
exploreNeighborsMany = state fun
  where fun s @ (_, _, 0, _) = ([], s)
        fun (w, ps, dist, visited) = (neighbors, newState)
          where exploreOnes = (\p -> (w, p, dist, visited)) <$> ps
                results = foldl accumulate ps
