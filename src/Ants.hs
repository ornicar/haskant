module Ants (game) where

import           Control.Applicative
import           Data.List           (isPrefixOf)
import           Data.Maybe          (mapMaybe)

import           Ai                  (DoTurn)
import           Protocol
import           System.IO
import           Util
import           World

updateGameFromInput :: GameParams -> GameState -> IO GameState
updateGameFromInput gp gs = do
  line <- getLine
  process line
  where process line
          | "turn" `isPrefixOf` line = do
              hPutStrLn stderr line
              updateGameFromInput gp gs
          | "go" `isPrefixOf` line = return gs
          | otherwise = updateGameFromInput gp $ updateGameState gs line

-- Clears ants and food and sets tiles to invisible
cleanState :: GameState -> GameState
cleanState gs = gs {world = clearTile <$> world gs, gameAnts = [], gameFoods = []}

gatherParamInput :: IO [String]
gatherParamInput = gatherInput' []
  where gatherInput' xs = do
          line <- getLine
          if "ready" /= line then gatherInput' (line:xs) else return xs

endGame :: IO ()
endGame = do
  players <- getLine
  hPutStrLn stderr $ "Number of players: " ++ (words players !! 1)
  scores <- getLine
  hPutStrLn stderr $ "Final scores: " ++ unwords (tail $ words scores)

gameLoop :: GameParams -> GameState -> DoTurn -> IO ()
gameLoop gp gs doTurn = do
  line <- getLine
  gameLoop' line
  where
    gameLoop' line
      | "turn" `isPrefixOf` line = do
          hPrint stderr line
          let gsc = cleanState gs
          gse <- updateGameFromInput gp gsc
          (ngs, orders) <- doTurn gse
          mapM_ (putStrLn . issueOrder) orders
          -- mapM_ putStrLn $ showReachable (world ngs)
          -- mapM_ putStrLn $ showBorders ngs North green
          -- mapM_ putStrLn $ showBorders ngs West red
          -- mapM_ putStrLn $ showBorders ngs South blue
          -- mapM_ putStrLn $ showBorders ngs East gray
          finishTurn
          gameLoop gp ngs doTurn
      | "end" `isPrefixOf` line = endGame
      | otherwise = gameLoop gp gs doTurn -- ignore line

game :: DoTurn -> IO ()
game doTurn = do
  paramInput <- gatherParamInput
  let gp = createParams $ mapMaybe (tuplify2 . words) paramInput
  let gs = initialGameState gp
  finishTurn -- signal done with setup
  gameLoop gp gs doTurn

finishTurn :: IO ()
finishTurn = do
  putStrLn "go"
  hFlush stdout
