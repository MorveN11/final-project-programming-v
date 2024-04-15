
import TransitionTest (transitionsTest)
import RandomGenerationTest (randomTest)
import NewTilesTest (newTilesTest)

main :: IO ()
main = do
  putStrLn "Running tests..."
  sequence_ (randomTest++transitionsTest++newTilesTest)
  putStrLn "All tests passed!"
