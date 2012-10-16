{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module World (
    Owner (..)
  , Ant
  , Hill
  , World
  , Tile (..)
	, Content (..)
  , Order
  , Mission
  , Missions
  , Move
  , Ants
  , Orders
  , Tiles
	, isAnt
  , isMine
  , isHis
  , isOpen
  , tileOf
  , moveOpen
  , moveToOrder
  , directions
  , tileNeighbors
  , tileOpenNeighbors
  , pointOpenNeighbors
  , pointIsOpenAndFree
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
data Content = Mine | His | MyHill | HisHill | Land | Food | Water deriving (Show,Eq,Enum,Bounded)

data Tile = Tile { tilePoint :: Point, content :: Content, mystery :: Int }

instance Pointed Tile where point = tilePoint
instance Eq Tile where a == b = point a == point b
instance Show Tile where show a = (show . point) a ++ "{" ++ (show . content) a ++ "}"
instance Ord Tile where compare a b = compare (point a) (point b)

type World = Tore Tile

data Owner = Me | Him deriving (Show,Eq)

class Owned o where owner :: o -> Owner

type Ant = (Point, Owner)

type Hill = (Point, Owner)

instance Pointed (Point, Owner) where point = fst
instance Owned (Point, Owner) where owner = snd

type Order = (Point, Direction) -- ant, direction
type Mission = (Move, Points) -- (from, to), path
type Missions = [Mission]
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

pointOpenNeighbors :: Pointed p => World -> p -> [Point]
pointOpenNeighbors w p = point <$> tileOpenNeighbors w p

isAnt :: Content -> Bool
isAnt c = c == Mine || c == His

isOpen :: Content -> Bool
isOpen c = c /= Water && c /= MyHill

pointIsOpenAndFree :: Pointed p => World -> p -> Bool
pointIsOpenAndFree w p = ((`notElem` [Water, MyHill, Mine]) . content) $ w %! p

isMine :: Owned o => o -> Bool
isMine = (== Me) . owner

isHis :: Owned o => o -> Bool
isHis = not . isMine

tileOf :: Pointed p => World -> p -> Tile
tileOf w p = w %! point p

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
