module Ai (
    DoTurn
  , doTurn
) where

import           Control.Applicative
import           Control.Monad       (join, mfilter)
import           Data.List           ((\\), find)
import qualified Data.Map            as M
import           Data.Maybe          (mapMaybe, listToMaybe)
import qualified Data.Set            as S
import Data.Graph.AStar

import           Point
import           Protocol
import           Search
import           Tore
import           Util
import           World

type DoTurn = GameState -> IO (GameState, Orders)

type AntPoints = Points

type Tactic = GameState -> AntPoints -> Orders

exploreDist = 8
foodDist = 9
areaDist = 14

doTurn :: DoTurn
doTurn gs = do
    mapM_ putStrLn $ _showMissions keptMissions blue
    mapM_ putStrLn $ _showMissions newMissions red
    mapM_ putStrLn $ _showPoints (S.toList (borderSet gs3)) green
    return (gs3, finalOrders)
  where gs2 = updateMystery gs
        (orders, antPoints) = strategy [foodTactic, exploreTactic] gs2
        (keptMissions, newMissions) = borderMissions gs2 antPoints 
        missions = keptMissions ++ newMissions
        missionOrders = mapMaybe (missionOrder gs2) missions
        allOrders = orders ++ missionOrders
        finalOrders = preventCollisions (world gs2) allOrders
        gs3 = gs2 {gameMissions = updateMissions gs2 missions finalOrders}

missionOrder :: GameState -> Mission -> Maybe Order
missionOrder gs (from, to) = moveToOrder <$> maybeMove
  where w = world gs
        maybeMove = (,) from <$> (next >>= listToMaybe)
        next = aStar neighbors close (heur to) (== to) from
          where neighbors = S.fromList . pointOpenNeighbors w
                close _ _ = 1
                heur = manhattan (toreBound w)

updateMissions :: GameState -> Missions -> Orders -> Missions
updateMissions gs missions orders = updateMission <$> missions
  where updateMission m@(from, _) = maybe m withOrder $ find ((== from) . fst) orders
          where withOrder (_, dir) = (from, toreMove (world gs) from dir)

borderMissions :: GameState -> AntPoints -> (Missions, Missions)
borderMissions gs antPoints = (kepts, news)
  where kepts = mfilter keepMission (gameMissions gs)
        keepMission (from, _) = from `elem` antPoints
        freeAps = antPoints \\ (fst <$> kepts)
        news = mapMaybe (antMission . point) freeAps
        antMission a = (,) a <$> bfsClosestInSet (world gs) (borderSet gs) a

borderSet :: GameState -> S.Set Point
borderSet gs = bfsBorderSet (world gs) areaDist $ gameAnts gs

strategy :: [Tactic] -> GameState -> (Orders, AntPoints)
strategy fs gs = runTactics (($ gs) <$> fs) myAntPoints
  where myAntPoints = point <$> (isMine `filter` gameAnts gs)
        runTactics [] ants = ([], ants)
        runTactics _ [] = ([], [])
        runTactics (t:ts) ants = (orders ++ nextOrders, nextAnts)
          where (nextOrders, nextAnts) = runTactics ts remainingAnts
                orders = t ants
                remainingAnts = ants \\ (fst <$> orders)

exploreTactic :: Tactic
exploreTactic gs = mapMaybe exploreAnt 
  where w = world gs
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
