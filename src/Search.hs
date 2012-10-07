module Search(
    -- exploreDirection
    bfsTilesFrom
  , bfsTilesFroms
  , bfsMysteriousDir
  , bfsBorders
  , bfsMovesTo
) where

import           Control.Applicative
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
type Visited = [Point]

bfsMovesTo :: World -> Distance -> Foods -> [Ant] -> [Visited] -> [Move]
bfsMovesTo _ _ _ [] _ = []
bfsMovesTo _ _ [] _ _ = []
bfsMovesTo w d tos ants visited = undefined

-- gets all reachable tiles from several positions
bfsTilesFroms :: World -> Distance -> [Tile] -> TileSet
bfsTilesFroms w d tiles = S.unions $ bfsTilesFrom w d <$> tiles

-- gets all reachable tiles from a position
bfsTilesFrom :: World -> Distance -> Tile -> TileSet
bfsTilesFrom w dist tile = snd bfsResult
  where initState = (S.singleton tile, S.singleton tile)
        bfsResult = foldl distStep initState [1 .. dist]
        distStep (froms, visited) _ = (neighbors, neighbors `S.union` visited)
          where neighbors = S.unions $ bfsStep w visited <$> S.toList froms

-- gets the walkable direct neighbors and the updated visited set
bfsStep :: World -> Skip -> Tile -> TileSet
bfsStep w skip tile = S.difference (S.fromList $ tileOpenNeighbors w tile) skip

bfsMysteriousDir :: World -> Distance -> Tile -> Maybe Direction
bfsMysteriousDir w d t = listToMaybe mysteriousDirs <|> listToMaybe openDirs
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
                dirNeighbors (dns, vis) (dir, froms) = ((dir, ns) : dns, ns `S.union` vis)
                  where ns = S.unions $ bfsStep w vis <$> S.toList froms

bfsBorders :: World -> Distance -> Direction -> Tile -> [Tile]
bfsBorders w d dir t = maybe [] (S.toList . snd) northDirBorders
  where dirBorders = bfsDirBorders w d t
        northDirBorders = find ((dir ==) . fst) dirBorders
