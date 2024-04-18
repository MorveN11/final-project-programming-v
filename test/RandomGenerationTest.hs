module RandomGenerationTest (randomTest) where

import Game (Board, Tile (Empty, Tile))
import Logic (addRandomTile, findAvailableTileIndex, getIndexOfEmpties)
import Test.QuickCheck (Property, property, quickCheck, (===))

grid :: Board
grid =
  [ [Empty, Tile 4, Tile 3, Empty],
    [Empty, Empty, Tile 3, Empty],
    [Tile 10, Empty, Empty, Tile 3],
    [Empty, Empty, Empty, Tile 40]
  ]

emptyGrid :: Board
emptyGrid = replicate 4 (replicate 4 Empty)

testGetIndexOfEmpties :: Property
testGetIndexOfEmpties = property $ getIndexOfEmpties grid === [0, 3, 4, 5, 7, 9, 10, 12, 13, 14]

testRandomGeneration :: Property
testRandomGeneration =
  property $
    length (getIndexOfEmpties board) === 15
  where
    index = findAvailableTileIndex emptyGrid
    board = fst (addRandomTile index emptyGrid)

randomTest :: [IO ()]
randomTest =
  [ quickCheck testGetIndexOfEmpties,
    quickCheck testRandomGeneration
  ]
