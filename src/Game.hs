{-# LANGUAGE InstanceSigs #-}

module Game (Tile (..), Board, Model (..), Action (..),VisualBoard(..), TransitionTile(..),initialModel, gameSubs) where

import Constants (initialScore, size)
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

type Board = [[Tile]]
type VisualBoard  = [[TransitionTile]]

data Model = Model
  { board :: Board,
    visualBoard :: VisualBoard,
    score :: Int
  }

instance Eq Model where
  (==) :: Model -> Model -> Bool
  (Model board1 score1 _) == (Model board2 score2 _) = board1 == board2 && score1 == score2

data Action
  = Initialize
  | ArrowPress !Arrows
  deriving (Show, Eq)

gameSubs :: [Sub Action]
gameSubs = [directionSub ([38, 87], [40, 83], [37, 65], [39, 68]) ArrowPress]

emptyBoard :: a -> [[a]]
emptyBoard a = replicate size $ replicate size a

initialModel :: Model
initialModel =
  Model
    { board = emptyBoard Empty,
      visualBoard = emptyBoard TransitionTileEmpty,
      score = initialScore
    }
