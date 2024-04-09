{-# LANGUAGE RecordWildCards #-}

module Logic (updateModel) where

import Data.List (partition)
import Game (Action (..), Model (..), Tile (..))
import Miso (Effect, noEff)
import Miso.Subscription.Keyboard (Arrows (..))
import Utils (transpose)

updateModel :: Action -> Model -> Effect Action Model
updateModel Initialize m = noEff m
updateModel (ArrowPress Arrows {..}) Model {..} = noEff (Model {board = move (arrowX, arrowY) board, ..})

move :: (Int, Int) -> [[Tile]] -> [[Tile]]
move (x, y) boardGame
  | x == 0 && y == 0 = boardGame
  | x == 0 = transpose (map (move' (y * (-1))) (transpose boardGame))
  | otherwise = map (move' x) boardGame

move' :: Int -> [Tile] -> [Tile]
move' n list
  | n < 0 = tiles ++ empties
  | otherwise = empties ++ tiles
  where
    (empties, tiles) = partition (== Empty) list
