{-# LANGUAGE InstanceSigs #-}

module Game (Tile (..), Board, Model (..), Action (..), initialModel) where

import Constants (initialScore, size)

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
  | MoveUp
  | MoveDown
  | MoveLeft
  | MoveRight
  deriving (Show, Eq)

initialModel :: Model
initialModel =
  Model
    { board = replicate size $ replicate size Empty,
      score = initialScore
    }
