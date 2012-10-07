module AiTest(
  aiTests
) where

import           Test.HUnit

import           Ai
import           Protocol
import           World

aiTests = [
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
