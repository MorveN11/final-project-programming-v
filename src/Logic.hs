{-# LANGUAGE RecordWildCards #-}

module Logic (updateModel, getIndexOfEmpties, addRandomTile) where

import Collision (collision)
import Constants (size, tilesAmount, twoPercentChance)
import Data.List (partition)
import GHC.IO (unsafePerformIO)
import Game (Action (..), Board, Model (..), Tile (..))
import Miso (Effect, noEff)
import Miso.Subscription.Keyboard (Arrows (..))
import Utils (chop, getRandomInt, getValueOfVectorIndex, transpose)

updateModel :: Action -> Model -> Effect Action Model
updateModel Initialize Model {..} = noEff Model {board = initBoard board, ..}
updateModel (ArrowPress Arrows {..}) Model {..} =
  let newBoard = move (arrowX, arrowY) board
      finalBoard = collision newBoard (arrowX, arrowY)
      updatedModel =
        if board /= finalBoard
          then Model {board = addRandomTile finalBoard, score = score}
          else Model {board = finalBoard, score = score}
   in noEff updatedModel

initBoard :: Board -> Board
initBoard board
  | length (getIndexOfEmpties board) == tilesAmount = initBoard (addRandomTile board)
  | otherwise = addRandomTile board

getIndexOfEmpties :: Board -> [Int]
getIndexOfEmpties grid = [i | (i, x) <- zip [0 ..] (concat grid), x == Empty]

addRandomTile :: Board -> Board
addRandomTile board = unsafePerformIO $ do
  probability <- getRandomInt (1, 10)
  let value = if probability <= twoPercentChance then 2 else 4
  return $ updateTile board index value
  where
    index = findAvailableTileIndex board

findAvailableTileIndex :: Board -> Int
findAvailableTileIndex grid = unsafePerformIO $ do
  index <- getRandomInt (0, length empties - 1)
  return $ getValueOfVectorIndex index empties
  where
    empties = getIndexOfEmpties grid

updateTile :: Board -> Int -> Int -> Board
updateTile grid index value =
  chop size (xs ++ [Tile value] ++ tail ys)
  where
    (xs, ys) = splitAt index (concat grid)

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
