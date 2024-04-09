{-# LANGUAGE RecordWildCards #-}
module Rendering (viewModel, updateModel) where
import Logic (move)
import Game (Action (..), Board, Model (..), Tile (..))
import Miso (Effect, View, button_, div_,  noEff, onClick, style_, text)
import Miso.String (ms)
import qualified Data.Map as Map
import Miso.Subscription.Keyboard (Arrows(..))
updateModel :: Action -> Model -> Effect Action Model
updateModel Initialize m = noEff m
updateModel (ArrowPress  Arrows{..}) Model{..}= noEff (Model {board = move (arrowX,arrowY) board,..})


viewModel :: Model -> View Action
viewModel Model {..} =
  div_
    []
    [ div_
        [ style_ $
            fromList
              [ (ms "display", ms "grid"),
                (ms "grid-template-columns", ms "repeat(4, 1fr)"),
                (ms "grid-gap", ms "10px"),
                (ms "width", ms "400px"),
                (ms "background-color", ms "#bbada0"),
                (ms "border-radius", ms "6px"),
                (ms "padding", ms "10px"),
                (ms "margin", ms "0 auto")
              ]
        ]
        (map viewTile (concat board)),
      div_
        [ style_ $
            fromList
              [ (ms "text-align", ms "center"),
                (ms "font-size", ms "24px"),
                (ms "margin-top", ms "20px")
              ]
        ]
<<<<<<< HEAD
        [text $ ms ("Score: " ++ show score)],
      div_
        [ style_ $
            fromList
              [ (ms "display", ms "flex"),
                (ms "justify-content", ms "center"),
                (ms "margin-top", ms "20px")
              ]
        ]
        [ button_ [onClick MoveUp] [text $ ms "Up"],
          button_ [onClick MoveDown] [text $ ms "Down"],
          button_ [onClick MoveLeft] [text $ ms "Left"],
          button_ [onClick MoveRight] [text $ ms "Right"]
        ]
=======
        [text $ "Score: " <> ms (show score)]
>>>>>>> 8b065fd (movements with arrow keys have been added)
    ]

viewTile :: Tile -> View Action
viewTile Empty =
  div_
    [ style_ $
        fromList
          [ (ms "background-color", ms "#cdc1b4"),
            (ms "border-radius", ms "3px"),
            (ms "width", ms "90px"),
            (ms "height", ms "90px"),
            (ms "display", ms "flex"),
            (ms "justify-content", ms "center"),
            (ms "align-items", ms "center"),
            (ms "font-size", ms "45px"),
            (ms "color", ms "#cdc1b4")
          ]
    ]
<<<<<<< HEAD
    [text $ ms ""]
=======
    [text (ms "")]
>>>>>>> 8b065fd (movements with arrow keys have been added)
viewTile (Tile n) =
  div_
    [ style_ $
        fromList
          [ (ms "background-color", ms "#eee4da"),
            (ms "border-radius", ms "3px"),
            (ms "width", ms "90px"),
            (ms "height", ms "90px"),
            (ms "display", ms "flex"),
            (ms "justify-content", ms "center"),
            (ms "align-items", ms "center"),
            (ms "font-size", ms "45px"),
            (ms "font-weight", ms "bold")
          ]
    ]
    [text $ ms (show n)]
