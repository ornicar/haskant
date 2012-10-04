module World
  (
    Owner (..)
  , Ant (..)
  , Direction (..)
  , World
  , Tile (..)
	, Content (..)
  , myAnts -- return list of my Ants
  , hisAnts -- return list of visible enemy Ants
	, isAnt
  , antPoint
	, move
	, updateWorldTile
	, updateWorldContent
	, clearTile
	, showReachable	
  ) where

import           Control.Applicative
import           Data.Array
import           Text.Printf         (printf)

import           Tore

-- Objects appearing on the map
data Content = Mine | His | Land | Food | Water deriving (Show,Eq,Enum,Bounded)

data Tile = Tile
  { point   :: Point
  , content :: Content
  , explore :: Int
  } deriving (Show)

type World = Tore Tile

data Owner = Me | Him deriving (Show,Eq,Bounded,Enum)

data Ant = Ant Point Owner deriving (Show)

data Direction = North | East | South | West deriving (Bounded, Eq, Enum)

instance Show Direction where
  show North = "N"
  show East  = "E"
  show South = "S"
  show West  = "W"

_tileNeighbors :: World -> Tile -> [Tile]
_tileNeighbors w t = (w !) <$> pointNeighbors w (point t)

antPoint :: Ant -> Point
antPoint (Ant p _) = p

isAnt :: Content -> Bool
isAnt t = t `elem` [Mine, His]

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

move :: Direction -> Point -> Point
move dir p
  | dir == North = (row p - 1, col p)
  | dir == South = (row p + 1, col p)
  | dir == West  = (row p, col p - 1)
  | otherwise    = (row p, col p + 1)

updateWorldTile :: World -> Tile -> Point -> World
updateWorldTile w t p = w // [(p, t)]

updateWorldContent :: World -> Content -> Point -> World
updateWorldContent w c p = updateWorldTile w newTile p
  where newTile = (w %! p) {content = c}

fOr :: a -> [a -> Bool] -> Bool
fOr x = any ($x)

-- Resets Content to Land if it is currently occupied by food or ant
-- and makes the content invisible
clearTile :: Tile -> Tile
clearTile m
  | fOr (content m) [isAnt, (==Food)] = m {content = Land}
  | otherwise = m

showReachable :: World -> [String]
showReachable w = "v setFillColor 0 255 0 0.075" : showTiles
  where reachableTiles = filter ((==0) . explore) $ elems w
        showTiles = showTile <$> reachableTiles
        showTile t = printf "v tile %d %d" (row $ point t) (col $ point t)
