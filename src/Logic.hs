{-# LANGUAGE RecordWildCards #-}

module Logic (updateModel, getIndexOfEmpties,addRandomTile) where

import Constants (size, twoPercentChance,tilesAmount,initialAmountValue)
import Data.List (partition)
import Game (Action (..), Board, Model (..), Tile (..))
import Miso (Effect, noEff)
import Miso.Subscription.Keyboard (Arrows (..))
import Utils (chop, getRandomInt, getValueOfVectorIndex, transpose)


updateModel :: Action -> Model -> Effect Action Model
updateModel Initialize Model {..} = noEff Model{board = initBoard board, ..} 
updateModel (ArrowPress Arrows {..}) Model {..} =
  if board /= board'
    then noEff (Model {board = addRandomTile board', ..})
    else noEff (Model {..})
  where
    board' = move (arrowX, arrowY) board


initBoard :: Board -> Board
initBoard board  | length (getIndexOfEmpties board) == tilesAmount  = initBoard (addRandomTile board)
                 |otherwise = updateTile board (findAvailableTileIndex board) initialAmountValue


addRandomTile :: Board -> Board
addRandomTile board =  updateTile board index value
  where
    index = findAvailableTileIndex board
    value = if getRandomInt (1, 10) <= twoPercentChance then 2 else 4


findAvailableTileIndex :: Board -> Int 
findAvailableTileIndex grid =  getValueOfVectorIndex (getRandomInt (0, length empties - 1)) empties
              where 
                empties = getIndexOfEmpties grid


updateTile :: Board -> Int -> Int -> Board
updateTile grid index value =
  chop size (xs ++ [Tile value] ++ tail ys)
  where
    (xs, ys) = splitAt index (concat grid)


getIndexOfEmpties :: Board -> [Int]
getIndexOfEmpties grid = [i | (i, x) <- zip [0 ..] (concat grid), x == Empty]


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
