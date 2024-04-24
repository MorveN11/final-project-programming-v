{-# LANGUAGE RecordWildCards #-}

module Logic (updateModel, getIndexOfEmpties, addRandomTile, findAvailableTileIndex) where

import Collision (collision)
import Constants (initialScore, size, tilesAmount, twoPercentChance)
import Data.List (partition)
import GHC.IO (unsafePerformIO)
import Game (Action (..), Board, GameState (..), Model (..), Tile (..), TransitionTile (..), VisualBoard (..), emptyBoard)
import Miso (Effect, noEff)
import Miso.Subscription.Keyboard (Arrows (..))
import Transition (findNewTiles, initTransitionBoard, transition)
import Utils (chop, getRandomInt, getValueOfVectorIndex, transpose)

updateModel :: Action -> Model -> Effect Action Model
updateModel Initialize Model {..} = noEff Model {board = initialBoard, visualBoard = VisualBoard {transitionBoard = initTransitionBoard initialBoard, newTiles = emptyBoard Empty}, ..}
  where
    initialBoard = initBoard board
updateModel Restart Model {..} =
  updateModel Initialize Model {board = emptyBoard Empty, visualBoard = VisualBoard {transitionBoard = emptyBoard TransitionTileEmpty, newTiles = emptyBoard Empty}, score = initialScore, bestScore = bestScore', gameState = InProgress}
  where
    bestScore' = max score bestScore
updateModel (ArrowPress Arrows {..}) Model {..} =
  if arrowX == arrowY
    then noEff Model {visualBoard = VisualBoard {transitionBoard = initTransitionBoard board, newTiles = emptyBoard Empty}, ..}
    else noEff model
  where
    board' = move (arrowX, arrowY) board
    board'' = collision board' (arrowX, arrowY)
    board''' = fst board''
    index = findAvailableTileIndex board'''
    newTile = addRandomTile index board'''
    board'''' = fst newTile
    value = snd newTile
    calculateScore = snd board''
    gameState' = checkGameState board'''
    score' = score + calculateScore
    model
      | gameState /= InProgress || board == board''' = Model {gameState = gameState', ..}
      | board /= board''' && gameState' == InProgress =
          Model
            { board = board'''',
              visualBoard =
                VisualBoard
                  { transitionBoard = transition board board''' (arrowX, arrowY),
                    newTiles =
                      if arrowX == 0
                        then updateTile (transpose (findNewTiles (transpose board') (transpose board'''))) index value
                        else updateTile (findNewTiles board' board''') index value
                  },
              score = score',
              ..
            }
      | otherwise =
          Model
            { board = board''',
              score = score',
              gameState = gameState',
              visualBoard =
                VisualBoard
                  { transitionBoard = transition board board''' (arrowX, arrowY),
                    newTiles =
                      if arrowX == 0
                        then updateTile (transpose (findNewTiles (transpose board') (transpose board'''))) index value
                        else updateTile (findNewTiles board' board''') index value
                  },
              ..
            }

checkGameState :: Board -> GameState
checkGameState board
  | any (elem (Tile 2048)) board = Win
  | not (any (elem Empty) board) && not (canMerge board) = GameOver
  | otherwise = InProgress

canMerge :: Board -> Bool
canMerge board = any canMergeRow board || any canMergeRow (transpose board)

canMergeRow :: [Tile] -> Bool
canMergeRow (x : y : xs) = x == y || canMergeRow (y : xs)
canMergeRow _ = False

initBoard :: Board -> Board
initBoard board
  | length (getIndexOfEmpties board) == tilesAmount = initBoard board'
  | otherwise = board'
  where
    index = findAvailableTileIndex board
    board' = fst (addRandomTile index board)

getIndexOfEmpties :: Board -> [Int]
getIndexOfEmpties grid = [i | (i, x) <- zip [0 ..] (concat grid), x == Empty]

addRandomTile :: Int -> Board -> (Board, Int)
addRandomTile index board = unsafePerformIO $ do
  probability <- getRandomInt (1, 10)
  let value = if probability <= twoPercentChance then 2 else 4
  return (updateTile board index value, value)

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
  | x /= 0 && y /= 0 = boardGame
  | x == 0 = transpose (map (move' (y * (-1))) (transpose boardGame))
  | otherwise = map (move' x) boardGame

move' :: Int -> [Tile] -> [Tile]
move' n list
  | n < 0 = tiles ++ empties
  | otherwise = empties ++ tiles
  where
    (empties, tiles) = partition (== Empty) list
