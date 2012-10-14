{-# LANGUAGE FlexibleInstances #-}
module QueueTest ( tests ) where

import           Distribution.TestSuite

instance TestOptions (String, Bool) where
    name = fst
    options = const []
    defaultOptions _ = return (Options [])
    check _ _ = []

instance PureTestable (String, Bool) where
    run (n, result) _ | result = Pass
                      | otherwise = Fail (n ++ " failed!")

test :: (String, Bool) -> Test
test = pure

-- In actual usage, the instances 'TestOptions (String, Bool)' and
-- 'PureTestable (String, Bool)', as well as the function 'test', would be
-- provided by the test framework.

tests :: [Test]
tests =
    [ test ("bar-1", True)
    , test ("bar-2", False)
    ]
