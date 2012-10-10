module Util(
    Color
  , green
  , blue
  , red
  , black
  , fillColor
  , lineColor
  , drawPoint
  , drawArrow
  , drawDir
  , fOr
  , tuplify2
  , enumerate
  , _debug
  , _debugIt
  , _debugAs
  , _debugPrefix
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

_debugPrefix :: Show a => String -> a -> a
_debugPrefix prefix a = trace (printf "[%s] %s" prefix $ show a) a

enumerate :: (Enum a, Bounded a) => [a]
enumerate = [minBound .. maxBound]

type Color = String
transparency = "0.3"
green = "0 120 0"
blue = "0 0 120"
red = "120 0 0"
black = "0 0 0"

fillColor :: Color -> String
fillColor = printf "v setFillColor %s 0.3" 

lineColor :: Color -> String
lineColor = printf "v setLineColor %s 1" 

drawCoords :: Int -> Int -> String
drawCoords = printf "v tile %d %d"

drawPoint :: Point -> String
drawPoint p = drawCoords (row p) (col p)

drawArrow :: Point -> Point -> String
drawArrow (r1, c1) (r2, c2) = printf "v arrow %d %d %d %d" r1 c1 r2 c2

drawDir :: Point -> String -> String
drawDir (r, c) = printf "v routePlan %d %d %s" r c 

average :: (Real a, Fractional b) => [a] -> b
average [] = 0
average xs = realToFrac (sum xs) / genericLength xs
