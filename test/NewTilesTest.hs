module NewTilesTest (newTilesTest) where 

import Game (Board, Tile (Empty, Tile))
import Logic (addRandomTile, getIndexOfEmpties)
import Test.QuickCheck (Property, property, quickCheck, (===))

import Transition (findNewTiles,findNewTiles')
import Game (Board,VisualBoard(..),TransitionTile (..), Tile (Empty, Tile))
import Foreign.C (eREMOTE)


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

newTilesAfterLeftMove :: Property
newTilesAfterLeftMove = property $ findNewTiles' [Tile 2,Tile 2,Tile 3,Empty] [Tile 4,Tile 3,Empty,Empty] === [Tile 4,Empty,Empty,Empty] 

newTilesAfterInitialLeftMove :: Property
newTilesAfterInitialLeftMove = property $ findNewTiles' [Empty,Tile 2,Tile 2,Tile 2] [Empty,Empty,Tile 2,Tile 4] === [Empty,Empty,Empty,Tile 4] 

newTilesStaticMov :: Property
newTilesStaticMov = property $ findNewTiles' [Tile 2,Tile 3,Empty,Empty] [Tile 2,Tile 3,Empty,Empty] === [Empty,Empty,Empty,Empty] 

newTilesStaticMov1 :: Property
newTilesStaticMov1 = property $ findNewTiles' [Empty,Empty,Tile 4,Tile 4] [Empty,Empty,Empty,Tile 8] === [Empty,Empty,Empty,Tile 8] 

newTilesStaticMov2 :: Property
newTilesStaticMov2 = property $ findNewTiles' [Empty,Empty,Empty,Tile 4] [Empty,Empty,Empty,Tile 4] === [Empty,Empty,Empty,Empty] 


newTilesTest:: [IO()]
newTilesTest  = [ 
                quickCheck newTilesAfterLeftMove,
                quickCheck newTilesAfterInitialLeftMove ,
                quickCheck newTilesStaticMov,
                quickCheck newTilesStaticMov1,
                quickCheck newTilesStaticMov2
                    ]
