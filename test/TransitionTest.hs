module TransitionTest (transitionsTest,IndexedTile) where 

import Game (Board, Tile (Empty, Tile))
import Logic (addRandomTile, getIndexOfEmpties)
import Test.QuickCheck (Property, property, quickCheck, (===))

import Transition (transition,calculateDesplacement',calculateDesplacement, initTransitionBoard,IndexedTile)
import Game (Board,VisualBoard(..),TransitionTile (..), Tile (Empty, Tile))



grid :: [[IndexedTile]]
grid =
  [ [(Empty,0,0), (Empty,1,0), (Empty,2,0), (Empty,3,0)],
    [(Tile 3,0,0), (Tile 3,1,0), (Tile 2,2,0), (Tile 2,3,0)],
    [(Tile 10,0,0), (Empty,1,0), (Empty,2,0), (Tile 3,3,0)],
    [(Empty,0,0), (Empty,1,0), (Empty,2,0), (Empty,0,0)]
  ]

gridDesplacedLeft :: [[IndexedTile]]
gridDesplacedLeft =
  [ [],
    [(Tile 6,0,0), (Tile 4,1,0)],
    [(Tile 10,0,0), (Tile 3,1,0)],
    []
  ]







moveListLeft :: Property
moveListLeft = property $ calculateDesplacement'  [(Tile 10,0,0), (Empty,1,0), (Tile 3,2,0), (Tile 3,3,0)] [(Tile 10,0,0), (Tile 6,1,0)] (-1,0) === [TransitionTile 10 (0,0),TransitionTileEmpty,TransitionTile  3 (-1,0),TransitionTile  3 (-2,0)]

moveListLeft2:: Property                                            
moveListLeft2 = property $ calculateDesplacement' [(Tile 3,0,0),(Tile 3,1,0),(Tile 3,2,0),(Tile 3,3,0)] [(Tile 6,0,0),(Tile 6,1,0)] (-1,0) === [TransitionTile 3 (0,0), TransitionTile  3 (-1,0), TransitionTile 3 (-1,0), TransitionTile 3 (-2,0)]


moveListLeft3:: Property                                            
moveListLeft3 = property $ calculateDesplacement' [(Empty,0,0),(Empty,1,0),(Empty,2,0),(Empty,3,0)] [] (-1,0) === [TransitionTileEmpty,TransitionTileEmpty,TransitionTileEmpty,TransitionTileEmpty]


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
                     quickCheck initialBoardParserTest
                     ]
