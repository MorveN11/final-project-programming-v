module Collision (collision) where

import Game ( Tile (..), Board )
import Utils (transpose)

{-data Tile = Empty | Tile Int deriving (Show, Eq)

type Board = [[Tile]]

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([] : _) = []
transpose rows = map head rows : transpose (map tail rows)-}

mayCollide :: Tile -> Tile -> Bool
mayCollide tile1 tile2
  | tile1 == Empty || tile2 == Empty = False
  | otherwise = tile1 == tile2

collisionTiles :: Tile -> Tile -> Tile
collisionTiles (Tile value1) (Tile value2) = Tile (value1 + value2)

collision :: Board -> (Int, Int) -> Board
collision board (x, y)
  | x < 0 && abs x > length board = board
  | y < 0 && abs y > length (head board) = board
  | otherwise = case (x, y) of
      (-1, 0) -> map (fillRow . mergeRow) board --izquierda
      (1, 0) -> map (fillRow . reverse . mergeRow . reverse) board --derecha
      (0, 1) -> transpose (map (fillRow . mergeRow) (transpose board)) --arriba
      (0, -1) -> transpose (map (fillRow . reverse . mergeRow . reverse) (transpose board)) --abajo
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