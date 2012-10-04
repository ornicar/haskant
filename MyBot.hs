module Main where

import Ants (game)
import Ai (doTurn)
import Algo

main :: IO ()
main = game doTurn
