module World
  (
    Owner (..)
  , Ant (..)
  , Direction (..)
  , World
  , Tile (..)
	, Content (..)
  , TileTest
  , myAnts -- return list of my Ants
  , hisAnts -- return list of visible enemy Ants
	, isAnt
  , isMine
  , isOpen
  , antPoint
  , antTile
	, move
  , moveOpen
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

import           Tore
import           Util

-- Objects appearing on the map
data Content = Mine | His | Land | Food | Water deriving (Show,Eq,Enum,Bounded)

data Tile = Tile { point :: Point, content :: Content, mystery :: Int }

instance Eq Tile where a == b = point a == point b
instance Show Tile where show a = (show . point) a ++ "{" ++ (show . content) a ++ "}"
instance Ord Tile where compare a b = compare (point a) (point b)

type World = Tore Tile

data Owner = Me | Him deriving (Show,Eq,Bounded,Enum)

data Ant = Ant Point Owner deriving (Show)

data Direction = North | East | South | West deriving (Bounded, Eq, Enum)

type TileTest = Tile -> Bool

instance Show Direction where
  show North = "N"
  show East  = "E"
  show South = "S"
  show West  = "W"

directions :: [Direction]
directions = enumerate

tileNeighbors :: World -> Tile -> [Tile]
tileNeighbors w t = (w !) <$> pointNeighbors w (point t)

tileOpenNeighbors :: World -> Tile -> [Tile]
tileOpenNeighbors w t = filter (isOpen . content) $ tileNeighbors w t

antPoint :: Ant -> Point
antPoint (Ant p _) = p

antTile :: World -> Ant -> Tile
antTile w a = w ! antPoint a

isAnt :: Content -> Bool
isAnt c = c `elem` [Mine, His]

isOpen :: Content -> Bool
isOpen c = c /= Water

_oneNorm :: Point -> Int
_oneNorm p = row p + col p

isMine :: Ant -> Bool
isMine (Ant _ owner) = owner == Me

myAnts :: [Ant] -> [Ant]
myAnts = filter isMine

isHis :: Ant -> Bool
isHis = not . isMine

hisAnts :: [Ant] -> [Ant]
hisAnts = filter isHis

move :: Point -> Direction -> Point
move p dir
  | dir == North = (row p - 1, col p)
  | dir == South = (row p + 1, col p)
  | dir == West  = (row p, col p - 1)
  | otherwise    = (row p, col p + 1)

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
