import           Test.Framework                 (defaultMain, testGroup)
import           Test.Framework.Providers.HUnit (testCase)

import AiTest

tests = [
    testGroup "Ai" $ zipWith (testCase . show) [1 ..] aiTests
  ]

--------------------------------------------------
main = defaultMain tests
