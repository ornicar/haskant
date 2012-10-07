module Protocol (
    GameState (..)
  , GameParams (..)
  , Order
  , createParams
  , issueOrder
  , initialGameState
  , updateGameState
) where

import           Control.Applicative
import           Data.Array
import           Data.Char           (digitToInt)
import           Data.List           (isPrefixOf)
import           Data.Maybe          (fromJust, fromMaybe)

import           Tore
import           Util
import           World

data GameState = GameState
  { world :: World
  , gameAnts  :: [Ant]
  , gameFoods  :: [Point]
  } deriving (Show)

data GameParams = GameParams
  { rows          :: Int
  , cols          :: Int
  , viewradius2   :: Int
  , attackradius2 :: Int
  , spawnradius2  :: Int
  , viewPoints    :: [Point]
  } deriving (Show)

issueOrder :: Order -> String
issueOrder (ant, dir) = "o " ++ srow ++ " " ++ scol ++ " " ++ sdir
    where srow = (show . row . antPoint) ant
          scol = (show . col . antPoint) ant
          sdir = show dir

createParams :: [(String, String)] -> GameParams
createParams s =
  let lookup' key = read $ fromJust $ lookup key s
      twoNormSquared p = row p ^ 2 + col p ^ 2
      rs  = lookup' "rows"
      cs  = lookup' "cols"
      vr2 = lookup' "viewradius2"
      ar2 = lookup' "attackradius2"
      sr2 = lookup' "spawnradius2"
      mx  = truncate $ sqrt $ fromIntegral vr2
      vp' = (,) <$> [-mx..mx] <*> [-mx..mx]
      vp  = filter ((<= 2) . twoNormSquared) vp'
  in GameParams { rows          = rs
                , cols          = cs
                , viewradius2   = vr2
                , attackradius2 = ar2
                , spawnradius2  = sr2
                , viewPoints    = vp
                }

initialGameState :: GameParams -> GameState
initialGameState gp = GameState {world = w, gameAnts = [], gameFoods = []}
  where maxRows = rows gp - 1
        maxCols = cols gp - 1
        w = listArray ((0,0), (maxRows, maxCols)) [Tile (x, y) Land 0 | x <- [0..maxRows], y <- [0..maxCols]]

updateGameState :: GameState -> String -> GameState
updateGameState gs s = fromMaybe gs $ updateGameStateMaybe gs s

updateGameStateMaybe :: GameState -> String -> Maybe GameState
updateGameStateMaybe gs s
  | "f" `isPrefixOf` s = addFood gs <$> toPoint (tail s)
  | "w" `isPrefixOf` s = (updateWorld gs . updateWorldContent (world gs) Water) <$> toPoint (tail s)
  | "a" `isPrefixOf` s = addAnt gs (toOwner . digitToInt . last $ s) <$> (toPoint . init . tail) s
  | otherwise = Nothing -- ignore line
  where
    toPoint :: String -> Maybe Point
    toPoint = tuplify2 . map read . words
    toOwner 0 = Me
    toOwner _ = Him

addFood :: GameState -> Point -> GameState
addFood gs p = gs {world = newWorld, gameFoods = p:gameFoods gs}
  where tile = world gs %! p
        newWorld = updateWorldTile (world gs) (tile {content = Food}) p

addAnt :: GameState -> Owner -> Point -> GameState
addAnt gs own p = GameState {world = newWorld, gameAnts = newAnts, gameFoods = gameFoods gs}
  where newAnts   = Ant p own : gameAnts gs
        tile = world gs %! p
        newTile = tile {content = if own == Me then Mine else His}
        newWorld  = updateWorldTile (world gs) newTile p

updateWorld :: GameState -> World -> GameState
updateWorld gs w = gs {world = w}
