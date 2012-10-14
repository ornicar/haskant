import           Test.Framework                 (defaultMain, testGroup)

import           QueueTest

main = defaultMain [
    testGroup "Queue" queueTests
  ]
