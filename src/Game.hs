{-# LANGUAGE InstanceSigs #-}

module Game (Tile (..), Board, Model (..), Action (..), GameState (..), VisualBoard (..), TransitionBoard, TransitionTile (..), initialModel, gameSubs, emptyBoard) where

import Constants (aKey, dKey, down, initialBestScore, initialScore, left, right, sKey, size, up, wKey)
import Miso.Subscription.Keyboard (Arrows (..), directionSub)
import Miso.Types (Sub)

data Tile
  = Empty
  | Tile Int
  deriving (Show, Eq)

data TransitionTile
  = TransitionTile Int (Int, Int)
  | TransitionTileEmpty
  deriving (Show, Eq)

type TransitionBoard = [[TransitionTile]]

data GameState = InProgress | Win | GameOver
  deriving (Show, Eq)

type Board = [[Tile]]

data VisualBoard = VisualBoard
  { transitionBoard :: TransitionBoard,
    newTiles :: Board
  }

data Model = Model
  { board :: Board,
    visualBoard :: VisualBoard,
    score :: Int,
    bestScore :: Int,
    gameState :: GameState
  }

instance Eq Model where
  (==) :: Model -> Model -> Bool
  (Model board1 _ score1 bestScore1 gameState1) == (Model board2 _ score2 bestScore2 gameState2) =
    board1 == board2 && score1 == score2 && bestScore1 == bestScore2 && gameState1 == gameState2

data Action
  = Initialize
  | Restart
  | ArrowPress !Arrows
  deriving (Show, Eq)

gameSubs :: [Sub Action]
gameSubs = [directionSub ([up, wKey], [down, sKey], [left, aKey], [right, dKey]) ArrowPress]

{-
demoBoardWin :: Board
demoBoardWin =
  [ [Tile 2, Tile 4, Tile 8, Tile 16],
    [Tile 32, Tile 64, Tile 128, Tile 256],
    [Tile 512, Tile 1024, Tile 1024, Empty],
    [Tile 1024, Tile 1024, Empty, Empty]
  ]
-}

emptyBoard :: a -> [[a]]
emptyBoard a = replicate size $ replicate size a

initialVisualBoard :: VisualBoard
initialVisualBoard = VisualBoard {transitionBoard = emptyBoard TransitionTileEmpty, newTiles = emptyBoard Empty}

initialModel :: Model
initialModel =
  Model
    { board = emptyBoard Empty,
      visualBoard = initialVisualBoard,
      score = initialScore,
      bestScore = initialBestScore,
      gameState = InProgress
    }
