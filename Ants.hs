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
  , Tile

    -- Utility functions
  , myAnts -- return list of my Ants
  , hisAnts -- return list of visible enemy Ants
  , passable
  , distance

    -- main function
  , game
  ) where

import           Control.Applicative
import           Data.Array
import           Data.Char           (digitToInt)
import           Data.List           (isPrefixOf)
import           Data.Maybe          (fromJust, fromMaybe, mapMaybe)
import           Debug.Trace
import           System.IO
import           Text.Printf         (printf)

-- type synonyms
type Row = Int
type Col = Int
type Point = (Row, Col)
type World = Array Point Tile

colBound :: World -> Col
colBound = col . worldBounds

rowBound :: World -> Row
rowBound = row . worldBounds

worldBounds :: World -> Point
worldBounds = snd . bounds

-- Takes the modulus of the indices before accessing the array
(%!) :: World -> Point -> Tile
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

_neighbors :: World -> Point -> [Point]
_neighbors w (r,c) = (w %!%) <$> [(r - 1, c), (r, c - 1), (r + 1, c), (r, c + 1)] 

_tileNeighbors :: World -> Tile -> [Tile]
_tileNeighbors w t = (w !) <$> _neighbors w (point t)

-- Objects appearing on the map
data Content = Mine | His | Land | Food | Water deriving (Show,Eq,Enum,Bounded)

data Tile = Tile
  { point   :: Point
  , content :: Content
  , explore :: Int
  } deriving (Show)

data Owner = Me | Him deriving (Show,Eq,Bounded,Enum)

data Ant = Ant
  { antPoint :: Point
  , owner    :: Owner
  } deriving (Show)

data Direction = North | East | South | West deriving (Bounded, Eq, Enum)

instance Show Direction where
  show North = "N"
  show East  = "E"
  show South = "S"
  show West  = "W"

-- Representation of an order
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
  { loadtime      :: Int
  , turntime      :: Int
  , rows          :: Int
  , cols          :: Int
  , turns         :: Int
  , viewradius2   :: Int
  , attackradius2 :: Int
  , spawnradius2  :: Int
  , viewPoints    :: [Point]
  } deriving (Show)

--------------- Tile functions -------------------
isAnt :: Content -> Bool
isAnt t = t `elem` [Mine, His]

modDistance :: Int -> Int -> Int -> Int
modDistance n x y = min ((x - y) `mod` n) ((y - x) `mod` n)

manhattan :: Point -> Point -> Point -> Int
manhattan bound p1 p2 = rowd + cold
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

isMine :: Ant -> Bool
isMine a = owner a == Me

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

passable :: World -> Order -> Bool
passable w order =
  let newPoint = move (direction order) (antPoint $ ant order)
  in  content (w %! newPoint) /= Water

issueOrder :: Order -> String
issueOrder order = do
  let srow = (show . row . antPoint . ant) order
      scol = (show . col . antPoint . ant) order
      sdir = (show . direction) order
  "o " ++ srow ++ " " ++ scol ++ " " ++ sdir

finishTurn :: IO ()
finishTurn = do
  putStrLn "go"
  hFlush stdout

tuplify2 :: [a] -> Maybe (a,a)
tuplify2 [x,y] = Just (x,y)
tuplify2 _ = Nothing

addFood :: GameState -> Point -> GameState
addFood gs p = gs {world = newWorld, food = p:food gs}
  where tile = world gs %! p
        newWorld = updateWorldTile (world gs) (tile {content = Food}) p

addAnt :: GameParams -> GameState -> Owner -> Point -> GameState
addAnt _ gs own p = GameState {world = newWorld, ants = newAnts, food = food gs}
  where newAnts   = Ant {antPoint = p, owner = own}:ants gs
        tile = world gs %! p
        newTile = tile {content = if own == Me then Mine else His}
        newWorld  = updateWorldTile (world gs) newTile p

updateWorldTile :: World -> Tile -> Point -> World
updateWorldTile w t p = w // [(p, t)]

updateWorldContent :: World -> Content -> Point -> World
updateWorldContent w c p = updateWorldTile w newTile p
  where newTile = (w %! p) {content = c}

updateWorld :: GameState -> World -> GameState
updateWorld gs w = gs {world = w}

updateGameState :: GameParams -> GameState -> String -> GameState
updateGameState gp gs s = fromMaybe gs $ updateGameStateMaybe gp gs s

updateGameStateMaybe :: GameParams -> GameState -> String -> Maybe GameState
updateGameStateMaybe gp gs s
  | "f" `isPrefixOf` s = addFood gs <$> toPoint (tail s)
  | "w" `isPrefixOf` s = (updateWorld gs . updateWorldContent (world gs) Water) <$> toPoint (tail s)
  | "a" `isPrefixOf` s = addAnt gp gs (toOwner . digitToInt . last $ s) <$> (toPoint . init . tail) s
  | otherwise = Nothing -- ignore line
  where
    toPoint :: String -> Maybe Point
    toPoint = tuplify2 . map read . words
    toOwner 0 = Me
    toOwner _ = Him

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

updateExplore :: GameState -> GameState
updateExplore gs = gs { world = exploration <$> world gs }
  where increment t = t { explore = explore t + 1 }
        antClose t = any (closeBy (world gs) (point t) . antPoint) $ ants gs
        exploration t = if antClose t then t {explore = 0} else increment t

closeBy :: World -> Point -> Point -> Bool
closeBy w a b = manhattan (worldBounds w) a b < 10

_fAnd :: a -> [a -> Bool] -> Bool
_fAnd x = all ($x)

fOr :: a -> [a -> Bool] -> Bool
fOr x = any ($x)

-- Resets Content to Land if it is currently occupied by food or ant
-- and makes the content invisible
clearTile :: Tile -> Tile
clearTile m
  | fOr (content m) [isAnt, (==Food)] = m {content = Land}
  | otherwise = m

-- Clears ants and food and sets tiles to invisible
cleanState :: GameState -> GameState
cleanState gs = GameState {world = clearTile <$> world gs, ants = [], food = []}

gatherParamInput :: IO [String]
gatherParamInput = gatherInput' []
  where
    gatherInput' :: [String] -> IO [String]
    gatherInput' xs = do
      line <- getLine
      if "ready" /= line then gatherInput' (line:xs) else return xs

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
      vp  = filter ((<= 2) . twoNormSquared) vp'
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

initialGameState :: GameParams -> GameState
initialGameState gp = GameState {world = w, ants = [], food = []}
  where maxRows = rows gp - 1
        maxCols = cols gp - 1
        w = listArray ((0,0), (maxRows, maxCols)) [Tile (x, y) Land 0 | x <- [0..maxRows], y <- [0..maxCols]]

gameLoop :: GameParams -> GameState
         -> (GameParams -> GameState -> [Order])
         -> IO ()
gameLoop gp gs doTurn = do
  line <- getLine
  gameLoop' line
  where
    gameLoop' line
      | "turn" `isPrefixOf` line = do
          hPrint stderr line
          let gsc = cleanState gs
          gse <- updateExplore <$> updateGame gp gsc
          let orders = doTurn gp gse
          mapM_ (putStrLn . issueOrder) orders
          mapM_ putStrLn $ (showReachable . world) gse
          finishTurn
          gameLoop gp gse doTurn
      | "end" `isPrefixOf` line = endGame
      | otherwise = gameLoop gp gs doTurn -- ignore line

showReachable :: World -> [String]
showReachable w = "v setFillColor 0 255 0 0.075" : showTiles
  where reachableTiles = filter ((==0) . explore) $ elems w
        showTiles = showTile <$> reachableTiles
        showTile t = printf "v tile %d %d" (row $ point t) (col $ point t)

_debug :: Show a => a -> b -> b
_debug a = trace (show a)

game :: (GameParams -> GameState -> [Order]) -> IO ()
game doTurn = do
  paramInput <- gatherParamInput
  let gp = createParams $ mapMaybe (tuplify2 . words) paramInput
  let gs = initialGameState gp
  finishTurn -- signal done with setup
  gameLoop gp gs doTurn
