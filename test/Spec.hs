import NewTilesTest (newTilesTest)
import RandomGenerationTest (randomTest)
import TransitionTest (transitionsTest)

main :: IO ()
main = do
  putStrLn "Running tests..."
  sequence_ (randomTest ++ transitionsTest ++ newTilesTest)
  putStrLn "All tests passed!"
