module Collision (collision) where

import Game (Board, Tile (..))
import Utils (transpose)

mayCollide :: Tile -> Tile -> Bool
mayCollide tile1 tile2
  | tile1 == Empty || tile2 == Empty = False
  | otherwise = tile1 == tile2

collisionTiles :: Tile -> Tile -> Tile
collisionTiles (Tile value1) (Tile value2) = Tile (value1 + value2)
collisionTiles _ _ = Empty

collision :: Board -> (Int, Int) -> Board
collision board (positionX, positionY)
  | positionX < 0 && abs positionX > length board = board
  | positionY < 0 && abs positionY > length (head board) = board
  | otherwise = case (positionX, positionY) of
      (-1, 0) -> map (fillRow . mergeRow . mergeRow) board
      (1, 0) -> map (reverse . fillRow . mergeRow . mergeRow . reverse) board
      (0, 1) -> transpose (map (fillRow . mergeRow . mergeRow) (transpose board))
      (0, -1) -> transpose (map (reverse . fillRow . mergeRow . mergeRow . reverse) (transpose board))
      _ -> board

mergeRow :: [Tile] -> [Tile]
mergeRow [] = []
mergeRow [tile] = [tile]
mergeRow (tile1 : tile2 : tiles)
  | mayCollide tile1 tile2 =
      let collisionedRow = collisionTiles tile1 tile2 : tiles
       in collisionedRow
  | otherwise = tile1 : mergeRow (tile2 : tiles)

fillRow :: [Tile] -> [Tile]
fillRow tiles = if length tiles <= 4 then tiles ++ replicate (4 - length tiles) Empty else take 4 tiles
