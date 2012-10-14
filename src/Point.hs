{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Point (
    Pointed(point, row, col)
  , Row
  , Col
  , Point
  , Points
) where

type Row = Int
type Col = Int
type Point = (Row, Col)
type Points = [Point]

class Pointed p where 

  point :: p -> Point

  row :: p -> Row
  row = fst . point

  col :: p -> Col
  col = snd . point

instance Pointed Point where point = id
