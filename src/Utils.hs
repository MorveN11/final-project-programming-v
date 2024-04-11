module Utils (transpose, getRandomInt, chop, getValueOfMatrixIndex, getValueOfVectorIndex) where

import GHC.IO (unsafePerformIO)
import System.Random (randomRIO)

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([] : _) = []
transpose rows = map head rows : transpose (map tail rows)

getRandomInt :: (Int, Int) -> Int
getRandomInt (i, e) = unsafePerformIO (randomRIO (i, e) :: IO Int)

chop :: Int -> [a] -> [[a]]
chop _ [] = []
chop n xs = take n xs : chop n (drop n xs)

getValueOfMatrixIndex :: (Int, Int) -> [[a]] -> a
getValueOfMatrixIndex (x, y) grid = (grid !! x) !! y

getValueOfVectorIndex :: Int -> [a] -> a
getValueOfVectorIndex x grid = grid !! x
