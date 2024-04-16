module TransitionTest (transitionsTest) where 

import Game (Board, Tile (Empty, Tile))
import Logic (addRandomTile, getIndexOfEmpties)
import Test.QuickCheck (Property, property, quickCheck, (===))

import Transition (transition,calculateDesplacement',calculateDesplacement, initTransitionBoard,IndexedTile)
import Game (Board,VisualBoard(..),TransitionTile (..), Tile (Empty, Tile))



grid :: [[IndexedTile]]
grid =
  [ [(Empty,0), (Empty,1), (Empty,2), (Empty,3)],
    [(Tile 3,0), (Tile 3,1), (Tile 2,2), (Tile 2,3)],
    [(Tile 10,0), (Empty,1), (Empty,2), (Tile 3,3)],
    [(Empty,0), (Empty,1), (Empty,2), (Empty,3)]
  ]

gridDesplacedLeft :: [[IndexedTile]]
gridDesplacedLeft =
  [ [],
    [(Tile 6,0), (Tile 4,1)],
    [(Tile 10,0), (Tile 3,1)],
    []
  ]







moveListLeft :: Property
moveListLeft = property $ calculateDesplacement'  [(Tile 10,0), (Empty,1), (Tile 3,2), (Tile 3,3)] [(Tile 10,0), (Tile 6,1)] (-1,0) 0 === [TransitionTile 10 (0,0),TransitionTileEmpty,TransitionTile  3 (-1,0),TransitionTile  3 (-2,0)]

moveListLeft2:: Property                                            
moveListLeft2 = property $ calculateDesplacement' [(Tile 3,0),(Tile 3,1),(Tile 3,2),(Tile 3,3)] [(Tile 6,0),(Tile 6,1)] (-1,0) 0=== [TransitionTile 3 (0,0), TransitionTile  3 (-1,0), TransitionTile 3 (-1,0), TransitionTile 3 (-2,0)]


moveListLeft3:: Property                                            
moveListLeft3 = property $ calculateDesplacement' [(Empty,0),(Empty,1),(Empty,2),(Empty,3)] [] (-1,0) 0 === [TransitionTileEmpty,TransitionTileEmpty,TransitionTileEmpty,TransitionTileEmpty]


moveBoardLeft :: Property
moveBoardLeft = property $ calculateDesplacement grid gridDesplacedLeft (-1,0) === [[TransitionTileEmpty,TransitionTileEmpty,TransitionTileEmpty,TransitionTileEmpty],
                                                                                  [TransitionTile  3 (0,0), TransitionTile 3 (-1,0), TransitionTile 2 (-1,0), TransitionTile 2 (-2,0)],
                                                                                  [TransitionTile 10 (0,0),TransitionTileEmpty, TransitionTileEmpty,  TransitionTile 3 (-2,0)],
                                                                                  [TransitionTileEmpty,TransitionTileEmpty,TransitionTileEmpty,TransitionTileEmpty]]


gridBoard :: Board
gridBoard  =  [ [Empty,Tile 2,Tile 3,Tile 2],
                [Empty,Tile 2,Tile 5,Tile 2],
                [Empty,Empty,Empty,Empty],
                [Empty,Empty,Empty,Tile 2]]


gridDesplacedUp :: Board
gridDesplacedUp =
              [ [Empty,Tile 4,Tile 3,Tile 4],
                [Empty,Empty,Tile 5,Tile 2],
                [Empty,Empty,Empty,Empty],
                [Empty,Empty,Empty,Empty]]


moveBoardUp :: Property
moveBoardUp = property $ transition gridBoard gridDesplacedUp (0,1) === [[TransitionTileEmpty,TransitionTile 2 (0,0),TransitionTile 3 (0,0),TransitionTile 2 (0,0)],
                                                                              [TransitionTileEmpty, TransitionTile 2 (0,-1), TransitionTile 5 (0,0), TransitionTile 2 (0,-1)],
                                                                              [TransitionTileEmpty,TransitionTileEmpty, TransitionTileEmpty, TransitionTileEmpty],
                                                                              [TransitionTileEmpty,TransitionTileEmpty,TransitionTileEmpty,TransitionTile 2 (0,-2)]]




gridBoard1 :: Board
gridBoard1  =  [ [Empty,Empty,Empty,Empty],
                [Empty,Tile 2,Empty,Empty],
                [Empty,Empty,Empty,Tile 8],
                [Empty,Empty,Tile 2,Tile 4]]


gridDesplaced1Left :: Board
gridDesplaced1Left =
              [ [Empty,Empty,Empty,Empty],
                [Tile 2,Empty,Empty,Empty],
                [Tile 8,Empty,Empty,Empty],
                [Tile 2,Tile 4 ,Empty,Empty]]


moveBoard1Left :: Property
moveBoard1Left = property $ transition gridBoard1 gridDesplaced1Left (-1,0) === [[TransitionTileEmpty,TransitionTileEmpty, TransitionTileEmpty, TransitionTileEmpty],
                                                                              [TransitionTileEmpty,TransitionTile 2 (-1,0),TransitionTileEmpty, TransitionTileEmpty],
                                                                              [TransitionTileEmpty,TransitionTileEmpty, TransitionTileEmpty, TransitionTile 8 (-3,0)],
                                                                              [TransitionTileEmpty,TransitionTileEmpty,TransitionTile 2 (-2,0),TransitionTile 4 (-2,0)]]



gridBoard2 :: Board
gridBoard2  =  [ [Empty,Tile 2,Empty,Empty],
                [Empty,Empty,Empty,Empty],
                [Tile 8,Empty,Empty,Empty],
                [Tile 8,Tile 2,Empty,Empty]]


gridDesplaced2Up :: Board
gridDesplaced2Up =
              [ [Tile 16,Tile 4,Empty,Empty],
                [Empty,Empty,Empty,Empty],
                [Empty,Empty,Empty,Empty],
                [Empty,Empty,Empty,Empty]]


moveBoard2Up :: Property
moveBoard2Up = property $ transition gridBoard2 gridDesplaced2Up (0,1) === [[TransitionTileEmpty,TransitionTile 2 (0,0), TransitionTileEmpty, TransitionTileEmpty],
                                                                              [TransitionTileEmpty,TransitionTileEmpty,TransitionTileEmpty, TransitionTileEmpty],
                                                                              [TransitionTile 8 (0,-2),TransitionTileEmpty, TransitionTileEmpty, TransitionTileEmpty],
                                                                              [TransitionTile 8 (0,-3),TransitionTile 2 (0,-3),TransitionTileEmpty,TransitionTileEmpty]]


initialBoard :: Board
initialBoard =  [ [Empty,Tile 2,Tile 3,Tile 2],
                  [Empty,Empty,Empty,Empty],
                  [Empty,Empty,Empty,Empty],
                  [Empty,Empty,Empty,Empty]]

initialBoardParserTest :: Property
initialBoardParserTest = property $ initTransitionBoard initialBoard  === [[TransitionTileEmpty,TransitionTile 2 (0,0),TransitionTile 3 (0,0),TransitionTile 2 (0,0)],
                                                                              [TransitionTileEmpty, TransitionTileEmpty, TransitionTileEmpty, TransitionTileEmpty],
                                                                              [TransitionTileEmpty,TransitionTileEmpty, TransitionTileEmpty, TransitionTileEmpty],
                                                                              [TransitionTileEmpty,TransitionTileEmpty,TransitionTileEmpty,TransitionTileEmpty]]


transitionsTest:: [IO()]
transitionsTest  = [ quickCheck moveListLeft,
                     quickCheck moveListLeft2,
                     quickCheck moveListLeft3,
                     quickCheck moveBoardLeft,
                     quickCheck moveBoardUp,
                     quickCheck initialBoardParserTest,
                     quickCheck moveBoard1Left,
                     quickCheck moveBoard2Up
                     ]
