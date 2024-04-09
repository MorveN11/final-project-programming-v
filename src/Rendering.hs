module Rendering (viewModel, updateModel) where

import Game (Action (..), Board, Model (..), Tile (..))
import Miso (Effect, View, button_, div_, ms, noEff, onClick, style_, text)

updateModel :: Action -> Model -> Effect Action Model
updateModel Initialize m = noEff m
updateModel MoveUp m = noEff m
updateModel MoveDown m = noEff m
updateModel MoveLeft m = noEff m
updateModel MoveRight m = noEff m

viewModel :: Model -> View Action
viewModel Model {board, score} =
  div_
    []
    [ div_
        [ style_ $
            Map.fromList
              [ ("display", "grid"),
                ("grid-template-columns", "repeat(4, 1fr)"),
                ("grid-gap", "10px"),
                ("width", "400px"),
                ("background-color", "#bbada0"),
                ("border-radius", "6px"),
                ("padding", "10px"),
                ("margin", "0 auto")
              ]
        ]
        (map viewTile (concat board)),
      div_
        [ style_ $
            Map.fromList
              [ ("text-align", "center"),
                ("font-size", "24px"),
                ("margin-top", "20px")
              ]
        ]
        [text $ "Score: " <> ms (show score)],
      div_
        [ style_ $
            Map.fromList
              [ ("display", "flex"),
                ("justify-content", "center"),
                ("margin-top", "20px")
              ]
        ]
        [ button_ [onClick MoveUp] [text "Up"],
          button_ [onClick MoveDown] [text "Down"],
          button_ [onClick MoveLeft] [text "Left"],
          button_ [onClick MoveRight] [text "Right"]
        ]
    ]

viewTile :: Tile -> View Action
viewTile Empty =
  div_
    [ style_ $
        Map.fromList
          [ ("background-color", "#cdc1b4"),
            ("border-radius", "3px"),
            ("width", "90px"),
            ("height", "90px"),
            ("display", "flex"),
            ("justify-content", "center"),
            ("align-items", "center"),
            ("font-size", "45px"),
            ("color", "#cdc1b4")
          ]
    ]
    [text ""]
viewTile (Tile n) =
  div_
    [ style_ $
        Map.fromList
          [ ("background-color", "#eee4da"),
            ("border-radius", "3px"),
            ("width", "90px"),
            ("height", "90px"),
            ("display", "flex"),
            ("justify-content", "center"),
            ("align-items", "center"),
            ("font-size", "45px"),
            ("font-weight", "bold")
          ]
    ]
    [text $ ms (show n)]
