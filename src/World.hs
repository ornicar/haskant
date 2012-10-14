module World (
    Owner (..)
  , Ant
  , World
  , Tile (..)
	, Content (..)
  , Order
  , Mission
  , Move
  , Ants
  , Orders
  , Tiles
	, isAnt
  , isMine
  , isHis
  , isOpen
  , antTile
  , moveOpen
  , moveToOrder
  , directions
  , tileNeighbors
  , tileOpenNeighbors
	, updateWorldTile
	, updateWorldContent
	, clearTile
	, showReachable
  ) where

import           Control.Applicative
import           Data.Array

import           Point
import           Tore
import           Util

-- Objects appearing on the map
data Content = Mine | His | Land | Food | Water deriving (Show,Eq,Enum,Bounded)

data Tile = Tile { tilePoint :: Point, content :: Content, mystery :: Int }

instance Pointed Tile where point = tilePoint
instance Eq Tile where a == b = point a == point b
instance Show Tile where show a = (show . point) a ++ "{" ++ (show . content) a ++ "}"
instance Ord Tile where compare a b = compare (point a) (point b)

type World = Tore Tile

data Owner = Me | Him deriving (Show,Eq,Bounded,Enum)

type Ant = (Point, Owner)

type Order = (Point, Direction) -- ant, direction
type Mission = (Point, Point) -- ant, target
type Move = (Point, Point)
type Ants = [Ant]
type Orders = [Order]
type Tiles = [Tile]

directions :: [Direction]
directions = enumerate

moveToOrder :: Move -> Order
moveToOrder (p@(r1, c1), (r2, c2)) = (p, dir)
  where dir = if r1 == r2 then sameRow else sameCol
        sameRow
            | c1 == c2 - 1 = East
            | c1 == c2 + 1 = West
            | otherwise = if c1 == 0 then West else East
        sameCol
            | r1 == r2 - 1 = South
            | r1 == r2 + 1 = North
            | otherwise = if r1 == 0 then North else South

tileNeighbors :: Pointed p => World -> p -> [Tile]
tileNeighbors w p = (w !) <$> pointNeighbors w p

tileOpenNeighbors :: Pointed p => World -> p -> [Tile]
tileOpenNeighbors w p = filter (isOpen . content) $ tileNeighbors w p

isAnt :: Content -> Bool
isAnt c = c `elem` [Mine, His]

isOpen :: Content -> Bool
isOpen c = c /= Water

isMine :: Ant -> Bool
isMine = (== Me) . snd

isHis :: Ant -> Bool
isHis = not . isMine

antTile :: World -> Ant -> Tile
antTile w a = w %! fst a

moveOpen :: World -> Tile -> Direction -> Maybe Tile
moveOpen w tile dir = if (isOpen . content) t2 then Just t2 else Nothing
  where t2 = w %! move (point tile) dir

updateWorldTile :: World -> Tile -> Point -> World
updateWorldTile w t p = w // [(p, t)]

updateWorldContent :: World -> Content -> Point -> World
updateWorldContent w c p = updateWorldTile w newTile p
  where newTile = (w %! p) {content = c}

-- Resets Content to Land if it is currently occupied by food or ant
-- and makes the content invisible
clearTile :: Tile -> Tile
clearTile m
  | fOr (content m) [isAnt, (==Food)] = m {content = Land}
  | otherwise = m

showReachable :: World -> [String]
showReachable w = fillColor blue : showTiles
  where reachableTiles = filter ((==0) . mystery) $ elems w
        showTiles = showTile <$> reachableTiles
        showTile = drawPoint . point
