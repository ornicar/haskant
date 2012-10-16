module Ai (
    DoTurn
  , doTurn
) where

import           Control.Applicative
import           Control.Monad       (join)
import           Data.List           ((\\))
import qualified Data.Map            as M
import           Data.Maybe          (mapMaybe)
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

exploreDist = 10
foodDist = 9
areaDist = 17

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
        missionOrders = mapMaybe missionOrder missions
        allOrders = orders ++ missionOrders
        finalOrders = preventCollisions (world gs2) allOrders
        updatedMissions = updateMissions missions finalOrders
        gs3 = gs2 {gameMissions = updatedMissions}

missionOrder :: Mission -> Maybe Order
missionOrder (_, []) = Nothing
missionOrder ((from, _), next:_) = Just $ moveToOrder (from, next)

-- path excludes starting and ending points
pathTo :: World -> Point -> Point -> Maybe Points
pathTo w from to = init <$> nextPath
  where nextPath = aStar neighbors close (heur to) (== to) from
        neighbors = S.fromList . pointOpenNeighbors w
        close _ _ = 1
        heur = manhattan (toreBound w)

updateMissions :: Missions -> Orders -> Missions
updateMissions missions orders = updateMission <$> missions
  where updateMission mission@(_, []) = mission
        updateMission mission@((from, to), next:path) 
          | any ((== from) . fst) orders = ((next, to), path)
          | otherwise = mission

borderMissions :: GameState -> AntPoints -> (Missions, Missions)
borderMissions gs antPoints = (kepts, news)
  where w = world gs
        kepts = filter keepMission (gameMissions gs)
        keepMission (_, []) = False
        keepMission ((from, to), _) = from `elem` antPoints && pointIsOpenAndFree w to 
        freeAps = antPoints \\ ((fst . fst) <$> kepts)
        news = mapMaybe (antMission . point) freeAps
        antMission a = do
          dest <- bfsClosestInSet w (borderSet gs) a
          path <- pathTo w a dest
          return ((a, dest), path)

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
  where showMission ((a, b), _) = drawArrow a b
