import Game (Board, Tile (Empty, Tile))
import Logic (addRandomTile, getIndexOfEmpties)
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
testRandomGeneration = property $ length (getIndexOfEmpties (addRandomTile emptyGrid)) === 15

testRandomGenerationMult :: Property
testRandomGenerationMult = property $ length (getIndexOfEmpties (addRandomTile (addRandomTile emptyGrid))) === 14

main :: IO ()
main = do
  putStrLn "Running tests..."
  sequence_
    [ quickCheck testGetIndexOfEmpties,
      quickCheck testRandomGeneration,
      quickCheck testRandomGenerationMult
    ]
  putStrLn "All tests passed!"
