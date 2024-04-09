{-# LANGUAGE RecordWildCards #-}

module Rendering (viewModel) where

import Data.Map (fromList)
import Game (Action (..), Model (..), Tile (..))
import Miso (View, div_, style_, text)
import Miso.String (ms)

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
        [text $ ms ("Score: " ++ show score)]
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
    [text $ ms ""]
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
