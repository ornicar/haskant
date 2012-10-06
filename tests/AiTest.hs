module AiTest(
  aiTests
) where

import           Test.HUnit

import           Ai
import           Protocol
import           World

aiTests = [
    case_testOrderPassable
    , case_testOrderNotPassable
  ]

--------------------------------------------------
-- | Simple game params used in conjunction with the example map
gameParams = createParams [
      ("rows","43"),
      ("cols","39"),
      ("viewradius2","77"),
      ("attackradius2","5"),
      ("spawnradius2","1")
  ]

make :: [String] -> GameState
make = foldl updateGameState $ initialGameState gameParams

--------------------------------------------------
testOrder gs makeAnt dir = orderPassable (world gs) (makeAnt gs, dir)

case_testOrderPassable = testOrder gs (head . ants) West @? "Not detecting an unoccupied tile" 
  where gs = make ["a 15 15 0","h 15 14 1","w 16 15","w 14 15","w 15 16"]

case_testOrderNotPassable = not (testOrder gs (head . ants) West) @? "Detecting a water tile" 
  where gs = make ["a 15 15 0","h 15 14 1","w 16 15","w 14 15","w 15 16","w 15 14"]
