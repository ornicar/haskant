module QueueTest(
  queueTests
) where

import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.HUnit

import           Data.List                            (nub)
import qualified Data.Set                             as S
import qualified SetQueue                             as Q

queueTests = [
    testProperty "list to queue to list" prop1,
    testCase "empty queue" test1,
    testProperty "enque removes duplicate elements" prop2,
    testProperty "deque removes duplicate elements" prop3,
    testProperty "queue set" prop4
  ]

type Que = Q.Q Int String

prop1 :: [Int] -> Bool
prop1 xs = (Q.toList . Q.fromList show) xs == xs

prop2 :: [Int] -> Bool
prop2 xs = Q.toList queue == nub xs
  where queue = foldl (flip Q.enque) emptyQueue xs

prop3 :: [Int] -> Bool
prop3 xs = dequeAll queue == nub xs
  where queue = foldl (flip Q.enque) emptyQueue xs
        dequeAll q = maybe [] withE $ Q.deque q 
        withE (x, nq) = x : dequeAll nq

prop4 :: [Int] -> Bool
prop4 xs = (Q.getSet . Q.fromList show) xs == S.fromList (map show xs)

test1 = Q.toList emptyQueue @=? []

emptyQueue :: Que
emptyQueue = Q.empty show
