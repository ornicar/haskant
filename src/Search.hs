module Search(
    -- exploreDirection
    bfsTilesFrom
  , bfsTilesFroms
) where

import           Control.Applicative
-- import           Data.List
-- import           Data.Maybe          (isJust, listToMaybe)
import qualified Data.Set            as S
-- import           Data.Tree
-- import           GHC.Exts            (sortWith)
-- import           Tore
import           World

type Distance = Int
type Position = Tile
type TileSet = S.Set Tile
type Visited = TileSet
type Skip = TileSet

-- gets all reachable tiles from several positions
bfsTilesFroms :: World -> Distance -> [Tile] -> TileSet
bfsTilesFroms w d tiles = S.unions $ bfsTilesFrom w d <$> tiles

-- gets all reachable tiles from a position
bfsTilesFrom :: World -> Distance -> Tile -> TileSet
bfsTilesFrom w dist tile = snd bfsResult
  where initState = (S.singleton tile, S.singleton tile)
        bfsResult = foldl distStep initState [0 .. dist]
        distStep (froms, visited) _ = (neighbors, neighbors `S.union` visited)
          where neighbors = S.unions $ bfsStep w visited <$> S.toList froms

-- gets the walkable direct neighbors and the updated visited set
bfsStep :: World -> Skip -> Tile -> TileSet
bfsStep w skip tile = S.difference (S.fromList $ tileOpenNeighbors w tile) skip
        
-- type Visited = [Tile]
-- type ExplorationOne = (World, Tile, Distance, Visited)

-- bfsIsReachable :: World -> Distance -> TileTest -> Tile -> Bool
-- bfsIsReachable w d test tile = isJust $ bfsFindOne test (w, tile, d, [tile])

-- bfsFindOne :: TileTest -> ExplorationOne -> Maybe Tile
-- bfsFindOne _ (_, _, 0, _) = Nothing
-- bfsFindOne test (w, tile, d, visited) = found <|> lookDeeper
--   where (neighbors, waters) = partition (isOpen . content) $ tileNeighbors w tile \\ visited
--         newVisited = waters ++ neighbors ++ visited
--         found = find test neighbors
--         nextExplorations = (\x -> (w, x, d - 1, newVisited)) <$> neighbors
--         lookDeeper = explorationTile <$> find (isJust . bfsFindOne test) nextExplorations

-- explorationTile :: ExplorationOne -> Tile
-- explorationTile (_, t, _, _) = t

-- _breadth :: Tree a -> [a]
-- _breadth nd =  map rootLabel $ nd : breadth' [nd]
--   where
--       breadth' [] = []
--       breadth' nds = let cs = foldr ((++).subForest) [] nds in
--         cs ++ breadth' cs

-- -- point surroundings as a rose tree
-- _surroundings :: Int -- distance
--   -> [Point] -- visited
--   -> World
--   -> Point -- start point
--   -> Tree Point
-- _surroundings dist visited w p = Node p forest
--   where neighbors = pointNeighbors w p \\ visited
--         newDist = dist - 1
--         newVisited = p : visited ++ neighbors
--         forest
--           | dist == 0 = []
--           | null neighbors = []
--           | otherwise = _surroundings newDist newVisited w <$> neighbors

-- _closeSurroundings :: World -> Point -> Tree Point
-- _closeSurroundings = _surroundings 2 []

-- _showSurrounding :: World -> String
-- _showSurrounding w = drawTree strTree
--   where strTree = show <$> tree
--         tree = _closeSurroundings w (10, 10)

-- exploreDirection :: World -> [Ant] -> [Maybe Direction]
-- exploreDirection w ants = pickBestDir <$> antsDirScores w ants
--   where pickBestDir ds = fst <$> listToMaybe (sortWith (negate . snd) ds)

-- type DirScore = (Direction, Int)
-- type DirScores = [DirScore]

-- antsDirScores :: World -> [Ant] -> [DirScores]
-- antsDirScores w ants = evaluateAnt <$> ants
--   where evaluateAnt ant = evaluate ant <$> directions
--         evaluate ant dir = (dir, 0)

-- exploreNeighborsOne :: ExplorationOne -> [ExplorationOne]
-- exploreNeighborsOne s @ (_, _, 0, _) = []
-- exploreNeighborsOne (w, p, dist, visited) = exploration <$> neighbors
--   where neighbors = pointNeighbors w p \\ visited
--         newVisited = p : visited ++ neighbors
--         exploration x = (w, x, dist - 1, newVisited)

-- type ExploreManyState = (World, [Position], Distance, Visited)
