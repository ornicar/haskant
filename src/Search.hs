module Search(
    bfsTilesFrom
  , bfsTilesFroms
  , bfsAttractiveDir
  , bfsBorders
  , bfsMovesTo
) where

import           Control.Applicative
import           Control.Monad       (join)
import           Data.List           (find)
import           Data.Maybe          (listToMaybe)
import qualified Data.Set            as S
import           GHC.Exts            (sortWith)
import           Tore
import           Util
import           World

type Distance = Int
type Position = Tile
type TileSet = S.Set Tile
type Skip = TileSet
type Visited = S.Set Tile
type Source = Tile
type Target = (Move, Point)
type Targets = [Target]
type Path = (Source, Tile)
type Paths = [Path]
type AntPoints = S.Set Point

bfsMovesTo :: World -> Distance -> [Source] -> Ants -> Targets
bfsMovesTo w d s ants = bfsMovesToAll w d S.empty (join (,) <$> s) makeAntPoints
  where makeAntPoints = S.fromList $ (\(Ant p _) -> p) <$> ants

type PathState = (Visited, Paths)

bfsMovesToAll :: World -> Distance -> Visited -> Paths -> AntPoints -> Targets
bfsMovesToAll _ _ _ [] _ = []
bfsMovesToAll _ 0 _ _ _ = []
bfsMovesToAll w dist visited paths ants | S.null ants = []
                                        | otherwise = targets ++ nextTargets
  where nextTargets = bfsMovesToAll w (dist - 1) newVis remainingPaths remainingAnts
        (newVis, newPaths) = visitPaths (visited, paths)
        (_, remainingAnts, targets) = applyPaths (newPaths, ants, [])
        remainingPaths = filter validSource newPaths
        validSource path = (point . fst) path `notElem` targetsPoints
        targetsPoints = snd <$> targets
        visitPaths :: PathState -> (Visited, Paths)
        visitPaths (vis, pp) = foldl visitPath (vis, []) pp
        visitPath :: (Visited, Paths) -> Path -> (Visited, Paths)
        visitPath (vis, pp) path = (vis `S.union` newTiles, nps ++ pp)
          where newTiles = S.fromList (tileOpenNeighbors w (snd path)) `S.difference` vis
                nps = (,) (fst path) <$> S.toList newTiles

-- for each path, try to add a target
-- and then remove the paths from the same source
-- and the used ant
applyPaths :: (Paths, AntPoints, Targets) -> (Paths, AntPoints, Targets)
applyPaths ([], ants, targets) = ([], ants, targets)
applyPaths ((source, tile):paths, ants, targets)
  | hasAnt = applyPaths (remainingPaths, remainingAnts, newTargets)
  | otherwise = (paths, ants, targets)
  where remainingAnts = S.delete (point tile) ants
        remainingPaths = filter ((/= source) . fst) paths
        newTargets = ((point tile, point tile), point source) : targets
        hasAnt = point tile `S.member` ants

-- gets all reachable tiles from several positions
bfsTilesFroms :: World -> Distance -> [Tile] -> TileSet
bfsTilesFroms w d tiles = S.unions $ bfsTilesFrom w d <$> tiles

-- gets all reachable tiles from a position
bfsTilesFrom :: World -> Distance -> Tile -> TileSet
bfsTilesFrom w dist tile = snd bfsResult
  where initState = (S.singleton tile, S.singleton tile)
        bfsResult = foldl distStep initState [1 .. dist]
        distStep (froms, visited) _ = (neighbors, visited `S.union` neighbors)
          where neighbors = S.unions $ bfsStep w visited <$> S.toList froms

-- gets the walkable direct neighbors and the updated visited set
bfsStep :: World -> Skip -> Tile -> TileSet
bfsStep w skip tile = S.difference (S.fromList $ tileOpenNeighbors w tile) skip

bfsAttractiveDir :: World -> Distance -> Tile -> Maybe Direction
bfsAttractiveDir w d t = listToMaybe mysteriousDirs <|> listToMaybe openDirs
  where dirInfos = bfsDirInfos w d t
        dirInfosDir (dir, _, _) = dir
        mysteriousDirs = dirInfosDir <$> filter (\(_, m, _) -> m > 0) sortedDirMysteries
        sortedDirMysteries = sortWith (\(_, m, _) -> -m) dirInfos
        openDirs = dirInfosDir <$> sortWith (\(_, _, o) -> -o) dirInfos

-- mystery average and number of open border tiles
type DirInfo = (Direction, Double, Int)

bfsDirInfos :: World -> Distance -> Tile -> [DirInfo]
bfsDirInfos w d t = dirInfos <$> bfsDirBorders w d t
  where dirInfos (dir, tileSet) = (dir, averageMystery, openess)
          where averageMystery = average $ (fromIntegral . mystery) <$> S.toList tileSet
                openess = S.size tileSet

type DirTiles = (Direction, TileSet)
type BfsState = ([DirTiles], TileSet)

bfsDirBorders :: World -> Distance -> Tile -> [DirTiles]
bfsDirBorders w dist tile = fst bfsResult
  where initDirTiles :: [DirTiles]
        initDirTiles  = dirTiles <$> directions
        dirTiles d = (d, maybe S.empty S.singleton (moveOpen w tile d))
        dirTilesToTiles dts = S.unions $ snd <$> dts
        initState :: BfsState
        initState = (initDirTiles, S.insert tile $ dirTilesToTiles initDirTiles)
        bfsResult :: BfsState
        bfsResult = foldl distStep initState [1 .. dist]
        distStep :: BfsState -> Int -> BfsState
        distStep (dirFroms, visited) _ = (neighbors, visited `S.union` dirTilesToTiles neighbors)
          where neighbors :: [DirTiles]
                neighbors = fst $ foldl dirNeighbors ([], visited) dirFroms
                dirNeighbors :: BfsState -> DirTiles -> BfsState
                dirNeighbors (dns, vis) (dir, froms) = ((dir, ns) : dns, vis `S.union` ns)
                  where ns = S.unions $ bfsStep w vis <$> S.toList froms

bfsBorders :: World -> Distance -> Direction -> Tile -> [Tile]
bfsBorders w d dir t = maybe [] (S.toList . snd) northDirBorders
  where dirBorders = bfsDirBorders w d t
        northDirBorders = find ((dir ==) . fst) dirBorders
