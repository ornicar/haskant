module Tore
  (
    Tore
  , Point
  , Points
  , Bound
  , Direction (..)
  , row
  , col
  , toreBound
  , (%!)
  , (%!%)
  , move
  , toreMove
  , pointNeighbors
  , manhattan
  ) where

import           Control.Applicative
import           Data.Array

import Point

type Bound = Point

type Tore a = Array Point a

data Direction = North | East | South | West deriving (Bounded, Eq, Enum)

instance Show Direction where
  show North = "N"
  show East  = "E"
  show South = "S"
  show West  = "W"

toreBound :: Tore a -> Bound
toreBound = snd . bounds

-- Takes the modulus of the indices before accessing the array
(%!) :: Pointed p => Tore a -> p -> a
(%!) t p = t ! (toreBound t %!% p)

(%!%) :: Pointed p => Bound -> p -> Point
(%!%) bound p = (ixRow, ixCol)
  where modCol = 1 + col bound
        modRow = 1 + row bound
        ixCol  = col p `mod` modCol
        ixRow  = row p `mod` modRow
 
move :: Pointed p => p -> Direction -> Point
move p North = (row p - 1, col p)
move p East  = (row p, col p + 1)
move p South = (row p + 1, col p)
move p West  = (row p, col p - 1)

toreMove :: Pointed p => Tore a -> p -> Direction -> Point
toreMove tore p dir = toreBound tore %!% move p dir

pointNeighbors :: Pointed p => Tore a -> p -> [Point]
pointNeighbors t p = (toreBound t %!%) <$> deltas
  where deltas = [(r - 1, c), (r, c - 1), (r + 1, c), (r, c + 1)]
        r = row p
        c = col p

manhattan :: Bound -> Point -> Point -> Int
manhattan b p1 p2 = rowd + cold
  where rowd = modDistance (row b) (row p1) (row p2)
        cold = modDistance (col b) (col p1) (col p2)

modDistance :: Int -- modulus
            -> Int -> Int -> Int
modDistance m x y = min a (m - a)
  where a = abs $ x - y
