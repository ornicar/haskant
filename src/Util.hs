module Util(
    Color
  , green
  , blue
  , red
  , gray
  , fillColor
  , drawPoint
  , fOr
  , tuplify2
  , enumerate
  , _debug
  , _debugIt
  , _debugAs
  , average
) where

import           Data.List   (genericLength)
import           Debug.Trace
import           Text.Printf (printf)
import           Tore

fOr :: a -> [a -> Bool] -> Bool
fOr x = any ($x)

tuplify2 :: [a] -> Maybe (a,a)
tuplify2 [x,y] = Just (x,y)
tuplify2 _ = Nothing

_debug :: Show a => a -> b -> b
_debug a = trace (show a)

_debugIt :: Show a => a -> a
_debugIt a = trace (show a) a

_debugAs :: Show b => (a -> b) -> a -> a
_debugAs f a = trace ((show . f) a) a

enumerate :: (Enum a, Bounded a) => [a]
enumerate = [minBound .. maxBound]

type Color = String
transparency = "0.3"
green = "0 120 0 " ++ transparency
blue = "0 0 120 "  ++ transparency
red = "120 0 0 " ++ transparency
gray = "0 0 0 " ++ transparency

fillColor :: Color -> String
fillColor c = "v setFillColor " ++ c

drawCoords :: Int -> Int -> String
drawCoords = printf "v tile %d %d"

drawPoint :: Point -> String
drawPoint p = drawCoords (row p) (col p)

average :: (Real a, Fractional b) => [a] -> b
average [] = 0
average xs = realToFrac (sum xs) / genericLength xs
