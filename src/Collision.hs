module Collision (collision) where

--import Constants (size)
--import Game (Board, Tile (..))
--import Utils (transpose)

size :: Int
size = 4

data Tile
  = Empty
  | Tile Int
  deriving (Show, Eq)

type Board = [[Tile]]

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([] : _) = []
transpose rows = map head rows : transpose (map tail rows)

mayCollide :: Tile -> Tile -> Bool
mayCollide tile1 tile2
  | tile1 == Empty || tile2 == Empty = False
  | otherwise = tile1 == tile2

collisionTiles :: Tile -> Tile -> (Tile, Int)
collisionTiles (Tile value1) (Tile value2) = (Tile (value1 + value2), value1 + value2)
collisionTiles _ _ = (Empty,0)

collision :: Board -> (Int, Int) -> (Board, Int)
collision board (positionX, positionY)
  | positionX < 0 && abs positionX > length board = (board, 0)
  | positionY < 0 && abs positionY > length (head board) = (board, 0)
  | otherwise = case (positionX, positionY) of
      (-1, 0) -> collisionLeft board
      (1, 0) -> collisionRight board
      (0, 1) -> collisionTop board
      (0, -1) -> collisionBottom board
      _ -> (board, 0)

collisionLeft :: Board -> (Board, Int)
collisionLeft board =
  let result = map (\row -> let (newRow, newRowScore) = mergeRow row
                            in (fillRow newRow, newRowScore)) board
      newBoard = map fst result
      newScore = sum (map snd result)
  in (newBoard, newScore)

collisionRight :: Board -> (Board, Int)
collisionRight board =
  let result = map (\row -> let (newRow, newRowScore) = mergeRow (reverse row)
                            in (fillRow newRow, newRowScore)) board
      newBoard = map (reverse . fst) result
      newScore = sum (map snd result)
  in (newBoard, newScore)

collisionBottom :: Board -> (Board, Int)
collisionBottom board =
  let result = map (\row -> let (newRow, newRowScore) = mergeRow (reverse row)
                            in (fillRow newRow, newRowScore)) (transpose board)
      newBoard = transpose (map (reverse . fst) result)
      newScore = sum (map snd result)
  in (newBoard, newScore)

collisionTop :: Board -> (Board, Int)
collisionTop board =
  let result = map (\row -> let (newRow, newRowScore) = mergeRow row
                            in (fillRow newRow, newRowScore)) (transpose board)
      newBoard = transpose (map fst result)
      newScore = sum (map snd result)
  in (newBoard, newScore)

mergeRow :: [Tile] -> ([Tile], Int)
mergeRow [] = ([], 0)
mergeRow [tile] = ([tile], 0)
mergeRow (tile1 : tile2 : tiles)
  | mayCollide tile1 tile2 =
      let collisionAndPoints = collisionTiles tile1 tile2
          collisionAux = mergeRow tiles
          collisionRow = fst collisionAndPoints : fst collisionAux
      in (collisionRow, snd collisionAndPoints + snd collisionAux)
  | otherwise =
    let result = mergeRow (tile2 : tiles)
        newRow = tile1 : fst result
    in (newRow, snd result)

fillRow :: [Tile] -> [Tile]
fillRow tiles = if length tiles <= size then tiles ++ replicate (size - length tiles) Empty else take size tiles
