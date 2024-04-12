{-# LANGUAGE RecordWildCards #-}

module Rendering (viewModel) where

import Data.Map (fromList)
import Game (Action (..), Model (..), Tile (..))
import Miso (View, button_, div_, h1_, p_, span_, style_, text)
import Miso.String (ms)

viewModel :: Model -> View Action
viewModel Model {..} =
  div_
    [ style_ $
        fromList
          [ (ms "background", ms "#faf8ef"),
            (ms "height", ms "100vh"),
            (ms "display", ms "flex"),
            (ms "justify-content", ms "center"),
            (ms "align-items", ms "center"),
            (ms "font-family", ms "Arial, sans-serif")
          ]
    ]
    [ div_
        [ style_ $
            fromList
              [ (ms "background", ms "#bbada0"),
                (ms "padding", ms "10px"),
                (ms "border-radius", ms "6px"),
                (ms "width", ms "400px"),
                (ms "margin-right", ms "40px")
              ]
        ]
        [ div_
            [ style_ $
                fromList
                  [ (ms "display", ms "grid"),
                    (ms "grid-template-columns", ms "repeat(4, 1fr)"),
                    (ms "border-color", ms "#7C6C64"),
                    (ms "grid-gap", ms "10px")
                  ]
            ]
            (map viewTile (concat board))
        ],
      div_
        [ style_ $
            fromList
              [ (ms "display", ms "flex"),
                (ms "flex-direction", ms "column"),
                (ms "align-items", ms "center")
              ]
        ]
        [ div_
            [ style_ $
                fromList
                  [ (ms "text-align", ms "center"),
                    (ms "margin-bottom", ms "20px")
                  ]
            ]
            [ h1_
                [ style_ $
                    fromList
                      [ (ms "font-size", ms "80px"),
                        (ms "font-weight", ms "bold"),
                        (ms "margin", ms "0")
                      ]
                ]
                [ span_ [style_ $ fromList [(ms "color", ms "#F65E3B")]] [text $ ms "2"],
                  span_ [style_ $ fromList [(ms "color", ms "#FFD02D")]] [text $ ms "0"],
                  span_ [style_ $ fromList [(ms "color", ms "#C93716")]] [text $ ms "4"],
                  span_ [style_ $ fromList [(ms "color", ms "#F2B179")]] [text $ ms "8"]
                ],
              p_
                [ style_ $
                    fromList
                      [ (ms "font-size", ms "18px"),
                        (ms "color", ms "#776e65"),
                        (ms "margin", ms "0")
                      ]
                ]
                [text $ ms "MISODEVS"]
            ],
          div_
            [ style_ $
                fromList
                  [ (ms "display", ms "flex"),
                    (ms "justify-content", ms "space-between"),
                    (ms "align-items", ms "center"),
                    (ms "width", ms "200px"),
                    (ms "margin-bottom", ms "20px")
                  ]
            ]
            [ div_
                [ style_ $
                    fromList
                      [ (ms "background", ms "#bbada0"),
                        (ms "padding", ms "5px 15px"),
                        (ms "border-radius", ms "3px"),
                        (ms "text-align", ms "center")
                      ]
                ]
                [ div_
                    [ style_ $
                        fromList
                          [ (ms "color", ms "#eee4da"),
                            (ms "font-size", ms "14px"),
                            (ms "margin-bottom", ms "5px")
                          ]
                    ]
                    [text $ ms "SCORE"],
                  div_
                    [ style_ $
                        fromList
                          [ (ms "color", ms "#ffffff"),
                            (ms "font-size", ms "24px"),
                            (ms "font-weight", ms "bold")
                          ]
                    ]
                    [text $ ms (show score)]
                ],
              div_
                [ style_ $
                    fromList
                      [ (ms "background", ms "#bbada0"),
                        (ms "padding", ms "5px 15px"),
                        (ms "border-radius", ms "3px"),
                        (ms "text-align", ms "center")
                      ]
                ]
                [ div_
                    [ style_ $
                        fromList
                          [ (ms "color", ms "#eee4da"),
                            (ms "font-size", ms "14px"),
                            (ms "margin-bottom", ms "5px")
                          ]
                    ]
                    [text $ ms "BEST SCORE"],
                  div_
                    [ style_ $
                        fromList
                          [ (ms "color", ms "#ffffff"),
                            (ms "font-size", ms "24px"),
                            (ms "font-weight", ms "bold")
                          ]
                    ]
                    [text $ ms "2048"]
                ]
            ],
          button_
            [ style_ $
                fromList
                  [ (ms "background", ms "#8f7a66"),
                    (ms "color", ms "#f9f6f2"),
                    (ms "border", ms "none"),
                    (ms "border-radius", ms "3px"),
                    (ms "padding", ms "10px 20px"),
                    (ms "font-size", ms "18px"),
                    (ms "font-weight", ms "bold"),
                    (ms "cursor", ms "pointer"),
                    (ms "font-family", ms "Arial, sans-serif")
                  ]
            ]
            [text $ ms "New Game"]
        ]
    ]

viewTile :: Tile -> View Action
viewTile Empty =
  div_
    [ style_ $
        fromList
          [ (ms "background", ms "#cdc1b4"),
            (ms "border-radius", ms "3px"),
            (ms "width", ms "90px"),
            (ms "height", ms "90px"),
            (ms "display", ms "flex"),
            (ms "justify-content", ms "center"),
            (ms "align-items", ms "center")
          ]
    ]
    []
viewTile (Tile n) =
  div_
    [ style_ $
        fromList
          [ (ms "background", ms $ getTileColor n),
            (ms "color", ms $ getTextColor n),
            (ms "border-radius", ms "3px"),
            (ms "width", ms "90px"),
            (ms "height", ms "90px"),
            (ms "display", ms "flex"),
            (ms "justify-content", ms "center"),
            (ms "align-items", ms "center"),
            (ms "font-size", ms "30px"),
            (ms "font-weight", ms "bold")
          ]
    ]
    [text $ ms (show n)]

getTileColor :: Int -> String
getTileColor n = case n of
  2 -> "#eee4da"
  4 -> "#ede0c8"
  8 -> "#f2b179"
  16 -> "#f59563"
  32 -> "#f67c5f"
  64 -> "#f65e3b"
  128 -> "#edcf72"
  256 -> "#edcc61"
  512 -> "#edc850"
  1024 -> "#edc53f"
  2048 -> "#edc22e"
  _ -> "#3c3a32"

getTextColor :: Int -> String
getTextColor n
  | n `elem` [2, 4] = "#776e65"
  | otherwise = "#f9f6f2"
