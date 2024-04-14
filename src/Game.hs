{-# LANGUAGE InstanceSigs #-}

module Game (Tile (..), Board, Model (..), Action (..), GameState (..), initialModel, gameSubs) where

import Constants (initialScore, size)
import Miso.Subscription.Keyboard (Arrows (..), directionSub)
import Miso.Types (Sub)

data Tile
  = Empty
  | Tile Int
  deriving (Show, Eq)

data GameState = InProgress | Win | GameOver
  deriving (Show, Eq)

type Board = [[Tile]]

data Model = Model
  { board :: Board,
    score :: Int,
    gameState :: GameState
  }

instance Eq Model where
  (==) :: Model -> Model -> Bool
  (Model board1 score1 gameState1) == (Model board2 score2 gameState2) = board1 == board2 && score1 == score2 && gameState1 == gameState2

data Action
  = Initialize
  | ArrowPress !Arrows
  deriving (Show, Eq)

gameSubs :: [Sub Action]
gameSubs = [directionSub ([38, 87], [40, 83], [37, 65], [39, 68]) ArrowPress]

{-
demoBoardWin :: Board
demoBoardWin =
  [ [Tile 2, Tile 4, Tile 8, Tile 16],
    [Tile 32, Tile 64, Tile 128, Tile 256],
    [Tile 512, Tile 1024, Tile 1024, Empty],
    [Tile 1024, Tile 1024, Empty, Empty]
  ]
-}

emptyBoard :: Board
emptyBoard = replicate size $ replicate size Empty

initialModel :: Model
initialModel =
  Model
    { board = emptyBoard,
      score = initialScore,
      gameState = InProgress
    }
