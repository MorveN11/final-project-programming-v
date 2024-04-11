import Game (Board, Tile (Empty, Tile))
import Logic (getIndexOfEmpties)
import Test.QuickCheck (Property, property, quickCheck, (===))

grid :: Board
grid =
  [ [Empty, Tile 4, Tile 3, Empty],
    [Empty, Empty, Tile 3, Empty],
    [Tile 10, Empty, Empty, Tile 3],
    [Empty, Empty, Empty, Tile 40]
  ]

testGetIndexOfEmpties :: Property
testGetIndexOfEmpties = property $ getIndexOfEmpties grid === [0, 3, 4, 5, 7, 9, 10, 12, 13, 14]

main :: IO ()
main = do
  putStrLn "Running tests..."
  quickCheck testGetIndexOfEmpties
  putStrLn "All tests passed!"
