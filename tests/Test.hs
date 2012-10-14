import           Test.Framework                 (defaultMain, testGroup)
import           Test.Framework.Providers.HUnit (testCase)

import QueueTest

tests = [
    testGroup "Queue" $ zipWith (testCase . show) [1 ..] queueTests
  ]

--------------------------------------------------
main = defaultMain tests
