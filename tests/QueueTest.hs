module QueueTest(
  queueTests
) where

import Test.Framework.Providers.QuickCheck2 (testProperty)

import SetQueue

queueTests = [
    testProperty "list to queue to list" prop_sort1
  ]

prop_sort1 :: [Int] -> Bool
prop_sort1 xs = (queueToList . listToQueue) xs == xs
