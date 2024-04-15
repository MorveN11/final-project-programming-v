
import TransitionTest (transitionsTest)
import RandomGenerationTest (randomTest)


main :: IO ()
main = do
  putStrLn "Running tests..."
  sequence_ (randomTest++transitionsTest)
  putStrLn "All tests passed!"
