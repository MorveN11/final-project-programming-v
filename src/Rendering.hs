{-# LANGUAGE RecordWildCards #-}

module Rendering (viewModel) where

import Data.Map (fromList)
import Game (Action (..), GameState (..), Model (..), Tile (..),TransitionTile (..), VisualBoard (..))
import Miso (View, button_, div_, h1_, onClick, p_, span_, style_, text,link_, href_, rel_,class_)
import Miso.String (ms,MisoString)


viewModel :: Model -> View Action
viewModel m@(Model {..}) =
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
    [ link_ [ rel_ (ms "stylesheet")
                 , href_ (ms "/home/fundacion/Documents/SEMESTER5/PROGRA/HASKELL/progra4/miso/learning/final-project-programming-v/static/style.css") ] ,
      div_
        [ style_ $
            fromList
              [ (ms "background", ms "#bbada0"),
                (ms "padding", ms "10px"),
                (ms "border-radius", ms "6px"),
                (ms "width", ms "400px"),
                (ms "margin-right", ms "40px"),
                (ms "position", ms "relative"),
                (ms "overflow", ms "hidden")
              ]
        ]
        [div_[style_ $ 
            fromList ([
              (ms "z-index", ms "3"),
              (ms "position", ms "sticky")
            ]++gridStyle)]
            (map viewTile (concat (transitionBoard visualBoard)))
          ,
          div_[style_ $
                fromList (gridStyle++
                [(ms "z-index", ms "2"),
                (ms "position", ms "absolute"),
              (ms "top", ms "10px"),
              (ms "left", ms "10px"),
              (ms "width", ms "400px")])]
            (replicate 16 tileCanvas)  
          ,
          div_ [
            style_ $ 
              fromList (gridStyle++[
              (ms "z-index", ms "4"),
              (ms "position", ms "absolute"),
              (ms "top", ms "10px"),
              (ms "left", ms "10px"),
              (ms "width", ms "400px")])
          ](map viewNewTiles (concat (newTiles visualBoard)))
          ,
          case gameState of
            Win -> viewOverlay "074003" "WIN :D"
            GameOver -> viewOverlay "C93716" "GAME OVER"
            InProgress -> div_ [] []
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
                    (ms "width", ms "260px"),
                    (ms "margin-bottom", ms "20px"),
                    (ms "gap", ms "10px")
                  ]
            ]
            [ div_
                [ style_ $
                    fromList
                      [ (ms "background", ms "#bbada0"),
                        (ms "padding", ms "5px 5px"),
                        (ms "border-radius", ms "3px"),
                        (ms "text-align", ms "center"),
                        (ms "width", ms "115px")
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
                        (ms "padding", ms "5px 5px"),
                        (ms "border-radius", ms "3px"),
                        (ms "text-align", ms "center"),
                        (ms "width", ms "115px")
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
                    [text $ ms bestScore]
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
                  ],
              onClick Restart
            ]
            [text $ ms "New Game"]
        ]
    ]


viewOverlay :: String -> String -> View Action
viewOverlay color message =
  div_
    [ style_ $
        fromList
          [ (ms "position", ms "absolute"),
            (ms "top", ms "0"),
            (ms "left", ms "0"),
            (ms "width", ms "100%"),
            (ms "height", ms "100%"),
            (ms "background", ms $ "#" ++ color ++ "A6"),
            (ms "display", ms "flex"),
            (ms "justify-content", ms "center"),
            (ms "align-items", ms "center")
          ]
    ]
    [ div_
        [ style_ $
            fromList
              [ (ms "padding", ms "10px 20px"),
                (ms "color", ms "#ffffff"),
                (ms "font-size", ms "30px"),
                (ms "font-weight", ms "bold")
              ]
        ]
        [text $ ms message]
    ]



viewNewTiles :: Tile -> View Action
viewNewTiles Empty = div_
    [ style_ $
        fromList tileStyle]
    []
viewNewTiles (Tile n) = div_
   [ style_ $
        fromList (tileStyle++[
          (ms "color", ms $ getTextColor n),
          (ms "position", ms "relative")
        ])]
    [
    span_ [
      class_ (ms "new-tile-number"),
      style_ $ fromList [ 
                          (ms "font-size", ms "1px"),
                          (ms "z-index", ms "50")]
            ] [text (ms (show n))],
    div_
    [ class_ (ms "new-tile-added")
      ,style_ $
        fromList
          [(ms "background", ms $ getTileColor n),
            (ms "border-radius", ms "0.03px"),
            (ms "width", ms "1px"),
            (ms "height", ms "1px"),
            (ms "display", ms "flex"),
            (ms "justify-content", ms "center"),
            (ms "align-items", ms "center"),
            (ms "font-weight", ms "bold"),
            (ms "position", ms "absolute"),
            (ms "top", ms "0"),
            (ms "left", ms "0"),
            (ms "right", ms "0"),
            (ms "bottom", ms "0"),
            (ms "z-index", ms "40")
            ]
    ]
    []
    ]

viewTile :: TransitionTile -> View Action
viewTile TransitionTileEmpty =  div_
    [ style_ $
        fromList tileStyle]
    []
viewTile (TransitionTile n (x,y)) =
  div_
    [ style_ $
        fromList
          ([ (ms "background", ms $ getTileColor n),
            (ms "color", ms $ getTextColor n)]
            ++tileStyle
            ++getTransition x y)
    ]
    [text $ ms (show n)]

getTransition :: Int -> Int  -> [(MisoString, MisoString)]
getTransition x y  | x == 0 && y == 0 = []
                  | otherwise = [
                    (ms "transition", ms "transform 0.5s ease"),
                    (ms "transform", ms $ "translate(" ++ show (fromIntegral x*102.7) ++ "px," ++ show (y*100) ++ "px)")
                  ] 

tileCanvas :: View Action
tileCanvas =
  div_
    [ style_ $
        fromList ((ms "background", ms "#cdc1b4"):tileStyle)] []

tileStyle :: [(MisoString, MisoString)]
tileStyle = [(ms "border-radius", ms "3px"),
            (ms "width", ms "90px"),
            (ms "height", ms "90px"),
            (ms "display", ms "flex"),
            (ms "justify-content", ms "center"),
            (ms "align-items", ms "center"),
            (ms "font-size", ms "30px"),
            (ms "font-weight", ms "bold")
            ]

gridStyle  :: [(MisoString, MisoString)]
gridStyle  = [ (ms "display", ms "grid"),
                    (ms "grid-template-columns", ms "repeat(4, 1fr)"),
                    (ms "border-color", ms "#7C6C64"),
                    (ms "grid-gap", ms "10px")
                  ]


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
