module Main where

import Ants (game)
import Ai (doTurn)

main :: IO ()
main = game doTurn
