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
  , euclidSquare
  , closeBy
  ) where

import           Control.Applicative
import           Data.Array
import qualified Data.Set as S

type Row = Int
type Col = Int
type Point = (Row, Col)
type Points = [Point]
type Bound = Point
type Tore a = Array Point a

data Direction = North | East | South | West deriving (Bounded, Eq, Enum)

instance Show Direction where
  show North = "N"
  show East  = "E"
  show South = "S"
  show West  = "W"

row :: Point -> Row
row = fst

col :: Point -> Col
col = snd

toreBound :: Tore a -> Bound
toreBound = snd . bounds

-- Takes the modulus of the indices before accessing the array
(%!) :: Tore a -> Point -> a
(%!) t p = t ! (toreBound t %!% p)

(%!%) :: Bound -> Point -> Point
(%!%) bound p = (ixRow, ixCol)
  where modCol = 1 + col bound
        modRow = 1 + row bound
        ixCol  = col p `mod` modCol
        ixRow  = row p `mod` modRow
 
move :: Point -> Direction -> Point
move p dir
  | dir == North = (row p - 1, col p)
  | dir == South = (row p + 1, col p)
  | dir == West  = (row p, col p - 1)
  | otherwise    = (row p, col p + 1)

toreMove :: Tore a -> Point -> Direction -> Point
toreMove tore p dir = toreBound tore %!% move p dir

pointNeighbors :: Tore a -> Point -> S.Set Point
pointNeighbors t (r,c) = S.fromList pointList
  where pointList = (toreBound t %!%) <$> [(r - 1, c), (r, c - 1), (r + 1, c), (r, c + 1)]

modDistance :: Int -> Int -> Int -> Int
modDistance n x y = min ((x - y) `mod` n) ((y - x) `mod` n)

manhattan :: Bound -> Point -> Point -> Int
manhattan bound p1 p2 = rowd + cold
  where rowd = modDistance (row bound + 1) (row p1) (row p2)
        cold = modDistance (col bound + 1) (col p1) (col p2)

euclidSquare :: Bound -> Point -> Point -> Int
euclidSquare bound p1 p2 =
  let rowd = modDistance (row bound + 1) (row p1) (row p2)
      cold = modDistance (col bound + 1) (col p1) (col p2)
  in (rowd ^ 2) + (cold ^ 2)

closeBy :: Bound -> Point -> Point -> Bool
closeBy bound a b = manhattan bound a b < 10
