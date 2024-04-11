{-# LANGUAGE RecordWildCards #-}

module Logic (updateModel, getIndexOfEmpties) where

import Constants (size, twoPercentChance)
import Data.List (partition)
import Game (Action (..), Board, Model (..), Tile (..))
import Miso (Effect, noEff)
import Miso.Subscription.Keyboard (Arrows (..))
import Utils (chop, getRandomInt, getValueOfVectorIndex, transpose)

updateTile :: Board -> Int -> Int -> Board
updateTile grid index value =
  chop size (xs ++ [Tile value] ++ tail ys)
  where
    (xs, ys) = splitAt index (concat grid)

getIndexOfEmpties :: Board -> [Int]
getIndexOfEmpties grid = [i | (i, x) <- zip [0 ..] (concat grid), x == Empty]

addRandomTile :: Model -> Model
addRandomTile Model {..} = Model {board = updateTile board index value, ..}
  where
    empties = getIndexOfEmpties board
    index = getValueOfVectorIndex (getRandomInt (0, length empties - 1)) empties
    value = if getRandomInt (1, 10) <= twoPercentChance then 2 else 4

updateModel :: Action -> Model -> Effect Action Model
updateModel Initialize m = noEff m
updateModel (ArrowPress Arrows {..}) Model {..} =
  if board /= board'
    then noEff (addRandomTile (Model {board = board', ..}))
    else noEff (Model {..})
  where
    board' = move (arrowX, arrowY) board

move :: (Int, Int) -> Board -> Board
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
