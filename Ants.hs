module Ants (game) where

import           Control.Applicative
import           Data.Array
import           Data.Char           (digitToInt)
import           Data.List           (isPrefixOf)
import           Data.Maybe          (fromJust, fromMaybe, mapMaybe)
import           Debug.Trace
import           System.IO

import           Tore
import           World

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

antPoint :: Ant -> Point
antPoint (Ant p _) = p

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
  where newAnts   = Ant p own : ants gs
        tile = world gs %! p
        newTile = tile {content = if own == Me then Mine else His}
        newWorld  = updateWorldTile (world gs) newTile p

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
        antClose t = any (closeBy ((toreBound . world) gs) (point t) . antPoint) $ ants gs
        exploration t = if antClose t then t {explore = 0} else increment t

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
      twoNormSquared p = row p ^ 2 + col p ^ 2
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
          let orders = filter (passable $ world gse) $ doTurn gp gse
          mapM_ (putStrLn . issueOrder) orders
          mapM_ putStrLn $ (showReachable . world) gse
          finishTurn
          gameLoop gp gse doTurn
      | "end" `isPrefixOf` line = endGame
      | otherwise = gameLoop gp gs doTurn -- ignore line

_debug :: Show a => a -> b -> b
_debug a = trace (show a)

game :: (GameParams -> GameState -> [Order]) -> IO ()
game doTurn = do
  paramInput <- gatherParamInput
  let gp = createParams $ mapMaybe (tuplify2 . words) paramInput
  let gs = initialGameState gp
  finishTurn -- signal done with setup
  gameLoop gp gs doTurn
