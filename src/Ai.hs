module Ai (
    DoTurn
  , doTurn
) where

import           Control.Applicative
import           Data.List           ((\\))
import qualified Data.Map            as M
import           Data.Maybe          (catMaybes)
import qualified Data.Set            as S

import           Control.Monad       (join)
import           Point
import           Protocol
import           Search
import           Tore
import           Util
import           World

type DoTurn = GameState -> IO (GameState, [Order])

type Tactic = GameState -> Points -> Orders

exploreDist = 7
foodDist = 8
areaDist = 14

doTurn :: DoTurn
doTurn gs = do
    mapM_ putStrLn $ _showMissions borderMissions red
    mapM_ putStrLn $ _showPoints (S.toList borderSet) green
    return (ngs, preventCollisions (world ngs) orders)
  where ngs = updateMystery gs
        w = world ngs
        (orders, ants) = strategy [foodTactic, exploreTactic, borderTactic] ngs
        borderSet = bfsBorderSet w areaDist $ gameAnts ngs
        borderMissions = catMaybes $ (antMission . point) <$> filter isMine (gameAnts ngs)
        antMission a = (,) a <$> bfsClosestInSet w borderSet a

loop :: [a -> b] -> a -> [b]
loop fs a = ($ a) <$> fs

strategy :: [Tactic] -> GameState -> (Orders, Points)
strategy fs gs = runTactics (($ gs) <$> fs) myAntPoints
  where myAntPoints = point <$> (isMine `filter` gameAnts gs)
        runTactics [] ants = ([], ants)
        runTactics _ [] = ([], [])
        runTactics (t:ts) ants = (orders ++ nextOrders, nextAnts)
          where (nextOrders, nextAnts) = runTactics ts remainingAnts
                orders = t ants
                remainingAnts = ants \\ (fst <$> orders)

borderTactic :: Tactic
borderTactic gs ants = []

exploreTactic :: Tactic
exploreTactic gs ants = catMaybes (exploreAnt <$> ants)
  where w = world gs
        exploreAnt :: Point -> Maybe Order
        exploreAnt a = (,) a <$> bfsMysteriousDir w exploreDist (w %! a)

foodTactic :: Tactic
foodTactic gs ants = (moveToOrder . fst) <$> foodTargets
  where w = world gs
        foodTargets = collectFoods ((w %!) <$> gameFoods gs) ants
        collectFoods = bfsMovesTo w foodDist

preventCollisions :: World -> [Order] -> [Order]
preventCollisions w orders = M.elems orderMap
  where orderMap = M.fromList $ (\o -> (uncurry (toreMove w) o, o)) <$> orders

updateMystery :: GameState -> GameState
updateMystery gs = gs { world = exploration <$> w }
  where w = world gs
        allReachableTiles = bfsTilesFroms w exploreDist (tileOf w <$> myAnts)
        myAnts = filter isMine (gameAnts gs)
        increment t = t { mystery = mystery t + 1 }
        reachable = flip S.member allReachableTiles
        exploration t = if reachable t then t {mystery = 0} else increment t

_showTargets :: [(Move, Point)] -> String -> [String]
_showTargets targets color = lineColor color : (targets >>= showTarget)
  where showTarget ((a, b), c) = drawArrow a <$> [b, c]

_showDirBorders :: GameState -> Direction -> String -> [String]
_showDirBorders gs dir color = fillColor color : showTiles
  where w = world gs
        antTiles = tileOf w <$> filter isMine (gameAnts gs)
        borderTiles = join $ bfsDirBorders w exploreDist dir <$> antTiles
        showTiles = showTile <$> borderTiles
        showTile = drawPoint . point

_showPoints :: [Point] -> String -> [String]
_showPoints points color = fillColor color : (drawPoint <$> points)

_showMissions :: [Mission] -> String -> [String]
_showMissions missions color = lineColor color : (showMission <$> missions) 
  where showMission (a, b) = drawArrow a b
