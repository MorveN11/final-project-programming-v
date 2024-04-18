module NewTilesTest (newTilesTest) where 

import Game (Tile (Empty, Tile))
import Test.QuickCheck (Property, property, quickCheck, (===))

import Transition (findNewTiles')




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
