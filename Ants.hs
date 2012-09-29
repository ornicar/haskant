module Ants
  (
    -- Data structures
    Owner (..)
  , Ant (..)
  , Direction (..)
  , GameParams (..)
  , GameState (..)
  , Order (..)
  , World

    -- Utility functions
  , myAnts -- return list of my Ants
  , enemyAnts -- return list of visible enemy Ants
  , passable
  , distance

    -- main function
  , game

  -- TODO implement the following functions according to the starter pack guide
  --, direction
  ) where

import Data.Array
import Data.List (isPrefixOf)
import Data.Char (digitToInt)
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
import Control.Applicative
import System.IO
import Debug.Trace

-- type synonyms
type Row = Int
type Col = Int
type Visible = Bool
type Point = (Row, Col)
type Food = Point
type World = Array Point MetaTile

colBound :: World -> Col
colBound = col . snd . bounds

rowBound :: World -> Row
rowBound = row . snd . bounds

-- Takes the modulus of the indices before accessing the array
(%!) :: World -> Point -> MetaTile
(%!) w p = w ! (w %!% p)

(%!%) :: World -> Point -> Point
(%!%) w p = (ixRow, ixCol)
  where modCol = 1 + colBound w
        modRow = 1 + rowBound w
        ixCol  = col p `mod` modCol
        ixRow  = row p `mod` modRow

row :: Point -> Row
row = fst

col :: Point -> Col
col = snd

-- Objects appearing on the map
data Tile = MyTile 
          | Enemy1Tile 
          | Enemy2Tile 
          | Enemy3Tile 
          | Dead 
          | Land 
          | FoodTile 
          | Water 
          | Unknown 
          deriving (Show,Eq,Enum,Bounded)

data MetaTile = MetaTile
  { tile :: Tile
  , visible :: Visible
  } deriving (Show)

data Owner = Me | Enemy1 | Enemy2 | Enemy3 deriving (Show,Eq,Bounded,Enum)

data Ant = Ant
  { point :: Point
  , owner :: Owner
  } deriving (Show)

data Direction = North | East | South | West deriving (Bounded, Eq, Enum)

instance Show Direction where
  show North = "N"
  show East  = "E"
  show South = "S"
  show West  = "W"

-- Representation of an order
data Order = Order
  { ant :: Ant
  , direction :: Direction
  } deriving (Show)

data GameState = GameState
  { world :: World
  , ants :: [Ant]
  , food :: [Food] 
  }

data GameParams = GameParams
  { loadtime :: Int
  , turntime :: Int
  , rows :: Int
  , cols :: Int
  , turns :: Int
  , viewradius2 :: Int
  , attackradius2 :: Int
  , spawnradius2 :: Int
  , viewPoints :: [Point]
  } deriving (Show)

--------------- Tile functions -------------------
isAnt :: Tile -> Bool
isAnt t = t `elem` [MyTile .. Enemy3Tile]

modDistance :: Int -> Int -> Int -> Int
modDistance n x y = min ((x - y) `mod` n) ((y - x) `mod` n)

_manhattan :: Point -> Point -> Point -> Int
_manhattan bound p1 p2 = rowd + cold
  where rowd = modDistance (row bound + 1) (row p1) (row p2)
        cold = modDistance (col bound + 1) (col p1) (col p2)

_oneNorm :: Point -> Int
_oneNorm p = row p + col p

twoNormSquared :: Point -> Int
twoNormSquared p = row p ^ 2 + col p ^ 2

_euclidSquare :: Point  -- bound
             -> Point -> Point -> Int
_euclidSquare bound p1 p2 = 
  let rowd = modDistance (row bound + 1) (row p1) (row p2)
      cold = modDistance (col bound + 1) (col p1) (col p2)
  in (rowd ^ 2) + (cold ^ 2)

distance :: GameParams -> Point -> Point -> Int
distance gp l1 l2 =
  let maxRow = rows gp - 1
      maxCol = cols gp - 1
      rowDist = modDistance maxRow (row l1) (row l2)
      colDist = modDistance maxCol (col l1) (col l2)
  in rowDist + colDist

isMe :: Ant -> Bool
isMe a = owner a == Me

myAnts :: [Ant] -> [Ant]
myAnts = filter isMe

isEnemy :: Ant -> Bool
isEnemy = not . isMe

enemyAnts :: [Ant] -> [Ant]
enemyAnts = filter isEnemy

move :: Direction -> Point -> Point
move dir p
  | dir == North = (row p - 1, col p)
  | dir == South = (row p + 1, col p)
  | dir == West  = (row p, col p - 1)
  | otherwise    = (row p, col p + 1)

passable :: World -> Order -> Bool
passable w order =
  let newPoint = move (direction order) (point $ ant order)
  in  tile (w %! newPoint) /= Water

issueOrder :: Order -> IO ()
issueOrder order = do
  let srow = (show . row . point . ant) order
      scol = (show . col . point . ant) order
      sdir = (show . direction) order
  putStrLn $ "o " ++ srow ++ " " ++ scol ++ " " ++ sdir

finishTurn :: IO ()
finishTurn = do
  putStrLn "go"
  hFlush stdout

tuplify2 :: [a] -> Maybe (a,a)
tuplify2 [x,y] = Just (x,y)
tuplify2 _ = Nothing

ownerToTile :: Owner -> Tile
ownerToTile Me = MyTile
ownerToTile Enemy1 = Enemy1Tile
ownerToTile Enemy2 = Enemy2Tile
ownerToTile Enemy3 = Enemy2Tile

toOwner :: Int -> Owner
toOwner 0 = Me
toOwner 1 = Enemy1
toOwner 2 = Enemy2
toOwner _ = Enemy3

addFood :: GameState -> Point -> GameState
addFood gs loc = 
  let newFood = loc:food gs
      newWorld = world gs // [(loc, MetaTile {tile = FoodTile, visible = True})]
  in GameState {world = newWorld, ants = ants gs, food = newFood}

sumPoint :: Point -> Point -> Point
sumPoint x y = (row x + row y, col x + col y)

addVisible :: World 
           -> [Point] -- viewPoints
           -> Point -- location
           -> World
addVisible w vp p = 
  let vis = map (sumPoint p) vp
      vtuple :: Point -> (Point, MetaTile)
      vtuple pt = (w %!% pt, visibleMetaTile $ w %! pt)
  in w // map vtuple vis

addAnt :: GameParams -> GameState -> Owner -> Point -> GameState
addAnt gp gs own p = 
  let newAnts   = Ant {point = p, owner = own}:ants gs
      newWorld' = if own == Me
                    then addVisible (world gs) (viewPoints gp) p
                    else world gs
      newWorld  = newWorld' // [(p, MetaTile {tile = ownerToTile own, visible = True})]
  in GameState {world = newWorld, ants = newAnts, food = food gs}

addDead :: GameParams -> GameState -> Owner -> Point -> GameState
addDead gp gs own p =
  let newWorld' = if own == Me 
                    then addVisible (world gs) (viewPoints gp) p
                    else world gs
      newWorld = newWorld' // [(p, MetaTile {tile = Dead, visible = True})]
  in GameState {world = newWorld, ants = ants gs, food = food gs}

-- if replacing a visible tile it should be kept visible
addWorldTile :: GameState -> Tile -> Point -> GameState
addWorldTile gs t p =
  let newWorld = world gs // [(p, MetaTile {tile = t, visible = True})]
  in GameState {world = newWorld, ants = ants gs, food = food gs}

initialGameState :: GameParams -> GameState
initialGameState gp =
  let w = listArray ((0,0), (rows gp - 1, cols gp - 1)) (repeat MetaTile {tile = Unknown, visible = False})
  in GameState {world = w, ants = [], food = []}

updateGameState :: GameParams -> GameState -> String -> GameState
updateGameState gp gs s = fromMaybe gs $ updateGameStateMaybe gp gs s

updateGameStateMaybe :: GameParams -> GameState -> String -> Maybe GameState
updateGameStateMaybe gp gs s
  | "f" `isPrefixOf` s = fmap (addFood gs) $ toPoint . tail $ s
  | "w" `isPrefixOf` s = fmap (addWorldTile gs Water) $ toPoint . tail $ s
  | "a" `isPrefixOf` s = fmap (addAnt gp gs $ toOwner . digitToInt . last $ s) $ toPoint . init . tail $ s
  | "d" `isPrefixOf` s = fmap (addDead gp gs $ toOwner . digitToInt . last $ s) $ toPoint . init . tail $ s
  | otherwise = Nothing -- ignore line
  where
    toPoint :: String -> Maybe Point
    toPoint = tuplify2 . map read . words

updateGame :: GameParams -> GameState -> IO GameState
updateGame gp gs = do
  line <- getLine
  process line
  where 
    process line
      | "turn" `isPrefixOf` line = do
          hPutStrLn stderr line
          updateGame gp gs 
      | "go" `isPrefixOf` line   = 
          return GameState {world = world gs
                           , ants = ants gs
                           , food = food gs
                           }
      | otherwise = updateGame gp $ updateGameState gp gs line

-- Sets the tile to visible
-- If the tile is still Unknown then it is land
visibleMetaTile :: MetaTile -> MetaTile
visibleMetaTile m 
  | tile m == Unknown = MetaTile {tile = Land, visible = True}
  | otherwise         = MetaTile {tile = tile m, visible = True}

_fAnd :: a -> [a -> Bool] -> Bool
_fAnd x = all ($x)

fOr :: a -> [a -> Bool] -> Bool
fOr x = any ($x)

-- Resets Tile to Land if it is currently occupied by food or ant
-- and makes the tile invisible
clearMetaTile :: MetaTile -> MetaTile
clearMetaTile m 
  | fOr (tile m) [isAnt, (==FoodTile), (==Dead)] = MetaTile {tile = Land, visible = False}
  | otherwise = MetaTile {tile = tile m, visible = False}

-- Clears ants and food and sets tiles to invisible
cleanState :: GameState -> GameState
cleanState gs = 
  GameState {world = nw, ants = [], food = []}
  where 
    w = world gs
    invisibles = map clearMetaTile $ elems w
    nw = listArray (bounds w) invisibles

gatherParamInput :: IO [String]
gatherParamInput = gatherInput' []
  where
    gatherInput' :: [String] -> IO [String]
    gatherInput' xs = do
      line <- getLine
      if "ready" /= line
        then gatherInput' (line:xs)
        else return xs
  
createParams :: [(String, String)] -> GameParams
createParams s =
  let lookup' key = read $ fromJust $ lookup key s
      lt  = lookup' "loadtime"
      tt  = lookup' "turntime"
      rs  = lookup' "rows"
      cs  = lookup' "cols"
      ts  = lookup' "turns"
      vr2 = lookup' "viewradius2"
      ar2 = lookup' "attackradius2"
      sr2 = lookup' "spawnradius2"
      mx  = truncate $ sqrt $ fromIntegral vr2
      vp' = (,) <$> [-mx..mx] <*> [-mx..mx]
      vp  = filter (\p -> twoNormSquared p <= vr2) vp'
  in GameParams { loadtime      = lt
                , turntime      = tt
                , rows          = rs
                , cols          = cs
                , turns         = ts
                , viewradius2   = vr2
                , attackradius2 = ar2
                , spawnradius2  = sr2
                , viewPoints    = vp
                }

endGame :: IO ()
endGame = do
  players <- getLine
  hPutStrLn stderr $ "Number of players: " ++ (words players !! 1)
  scores <- getLine
  hPutStrLn stderr $ "Final scores: " ++ unwords (tail $ words scores)
  -- TODO print 

gameLoop :: GameParams -> GameState
         -> (GameParams -> GameState -> [Order])
         -> IO ()
gameLoop gp gs doTurn = do
  line <- getLine
  gameLoop' line
  where
    gameLoop' line
      | "turn" `isPrefixOf` line = do 
          hPutStrLn stderr line
          let gsc = cleanState gs
          gsu <- updateGame gp gsc
          let orders = doTurn gp gsu
          mapM_ issueOrder orders
          finishTurn
          gameLoop gp gsu doTurn
      | "end" `isPrefixOf` line = endGame
      | otherwise = gameLoop gp gs doTurn -- ignore line

_debug :: Show a => a -> a
_debug a = trace (show a) a

game :: (GameParams -> GameState -> [Order]) -> IO ()
game doTurn = do
  paramInput <- gatherParamInput
  let gp = createParams $ mapMaybe (tuplify2 . words) paramInput
  let gs = initialGameState gp 
  finishTurn -- signal done with setup
  gameLoop gp gs doTurn
