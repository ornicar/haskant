module Tore
  (
    Tore
  , Point
  , Bound
  , row
  , col
  , toreBound
  , (%!)
  , (%!%)
  , pointNeighbors
  , manhattan
  , euclidSquare
  , closeBy
  ) where

import           Control.Applicative
import           Data.Array

type Row = Int
type Col = Int
type Point = (Row, Col)
type Bound = Point
type Tore a = Array Point a

row :: Point -> Row
row = fst

col :: Point -> Col
col = snd

toreBound :: Tore a -> Bound
toreBound = snd . bounds

-- Takes the modulus of the indices before accessing the array
(%!) :: Tore a -> Point -> a
(%!) w p = w ! (w %!% p)

(%!%) :: Tore a -> Point -> Point
(%!%) w p = (ixRow, ixCol)
  where colBound = col . toreBound
        rowBound = row . toreBound
        modCol = 1 + colBound w
        modRow = 1 + rowBound w
        ixCol  = col p `mod` modCol
        ixRow  = row p `mod` modRow

pointNeighbors :: Tore a -> Point -> [Point]
pointNeighbors w (r,c) = (w %!%) <$> [(r - 1, c), (r, c - 1), (r + 1, c), (r, c + 1)]

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
