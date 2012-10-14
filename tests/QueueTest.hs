module QueueTest(
  queueTests
) where

import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.HUnit

import qualified SetQueue                             as Q

queueTests = [
    testProperty "list to queue to list" prop1,
    testCase "empty queue" test1,
    testProperty "enque" prop2,
    testProperty "deque" prop3
  ]

prop1 :: [Int] -> Bool
prop1 xs = (Q.toList . Q.fromList) xs == xs

prop2 :: [Int] -> Bool
prop2 xs = Q.toList queue == xs
  where queue = foldl (flip Q.enque) emptyQueue xs

prop3 :: [Int] -> Bool
prop3 xs = dequeAll queue == xs
  where queue = foldl (flip Q.enque) emptyQueue xs
        dequeAll q = case Q.deque q of
          (Just x, nq) -> x : dequeAll nq
          (_, _) -> []

test1 = Q.toList emptyQueue @=? []

emptyQueue :: Q.Q Int
emptyQueue = Q.empty
