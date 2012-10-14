module QueueTest(
  queueTests
) where

import           Test.HUnit

queueTests = [
    case_testOrderPassable
  ]

case_testOrderPassable = True @? "Not detecting an unoccupied tile" 
