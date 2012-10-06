module Util(
    fOr
  , tuplify2
  , enumerate
) where

import           Debug.Trace

fOr :: a -> [a -> Bool] -> Bool
fOr x = any ($x)

tuplify2 :: [a] -> Maybe (a,a)
tuplify2 [x,y] = Just (x,y)
tuplify2 _ = Nothing

_debug :: Show a => a -> b -> b
_debug a = trace (show a)

enumerate :: (Enum a, Bounded a) => [a]
enumerate = [minBound .. maxBound]
