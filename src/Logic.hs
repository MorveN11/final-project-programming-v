module Logic (move) where

import Game (Tile(..))
import Data.List (partition)
import Utils (transpose)

move :: (Int , Int)-> [[Tile]] -> [[Tile]]
move (x,y)  boardGame | x == 0  && y == 0 = boardGame
                      | x == 0 =  transpose (map (move' (y*(-1))) (transpose boardGame))
                      | otherwise = map (move' x) boardGame

move' :: Int -> [Tile] -> [Tile]
move'  n list  | n<0 = tiles ++ empties
                         | otherwise = empties++tiles
                         where
                            (empties,tiles) = partition (==Empty) list
