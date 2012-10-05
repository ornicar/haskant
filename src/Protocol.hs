module Protocol (
    GameState (..)
  , GameParams (..)
  , Order (..)
) where

import           Tore
import           World

data Order = Order
  { ant       :: Ant
  , direction :: Direction
  } deriving (Show)

data GameState = GameState
  { world :: World
  , ants  :: [Ant]
  , food  :: [Point]
  } deriving (Show)

data GameParams = GameParams
  { rows          :: Int
  , cols          :: Int
  , viewradius2   :: Int
  , attackradius2 :: Int
  , spawnradius2  :: Int
  , viewPoints    :: [Point]
  } deriving (Show)
