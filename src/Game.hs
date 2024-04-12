{-# LANGUAGE InstanceSigs #-}

module Game (Tile (..), Board, Model (..), Action (..), initialModel, gameSubs) where

import Constants (initialScore, size)
import Miso.Subscription.Keyboard (Arrows (..), directionSub)
import Miso.Types (Sub)

data Tile
  = Empty
  | Tile Int
  deriving (Show, Eq)

type Board = [[Tile]]

data Model = Model
  { board :: Board,
    score :: Int
  }

instance Eq Model where
  (==) :: Model -> Model -> Bool
  (Model board1 score1) == (Model board2 score2) = board1 == board2 && score1 == score2

data Action
  = Initialize
  | ArrowPress !Arrows
  deriving (Show, Eq)

gameSubs :: [Sub Action]
gameSubs = [directionSub ([38, 87], [40, 83], [37, 65], [39, 68]) ArrowPress]

emptyBoard :: Board
emptyBoard = replicate size $ replicate size Empty

initialModel :: Model
initialModel =
  Model
    { board = emptyBoard,
      score = initialScore
    }
