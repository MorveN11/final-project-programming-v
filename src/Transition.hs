{-# LANGUAGE LambdaCase #-}

module Transition (transition, initTransitionBoard, calculateDisplacement', calculateDisplacement, findNewTiles, findNewTiles', IndexedTile) where

import Data.List (transpose)
import Game (Board, Tile (..), TransitionTile (..))

type IndexedTile = (Tile, Int)

initTransitionBoard :: Board -> [[TransitionTile]]
initTransitionBoard =
  map
    ( map
        ( \case
            Empty -> TransitionTileEmpty
            Tile t -> TransitionTile t (0, 0)
        )
    )

transition :: Board -> Board -> (Int, Int) -> [[TransitionTile]]
transition prevBoard newBoard arrows@(arrowX, _)
  | horizontal = transpose transitionedBoard
  | otherwise = transitionedBoard
  where
    horizontal = arrowX == 0
    indexedPrevBoard = indexTile prevBoard horizontal
    indexedNewBoard = filterTile (indexTile newBoard horizontal)
    transitionedBoard = calculateDisplacement indexedPrevBoard indexedNewBoard arrows

indexTile :: Board -> Bool -> [[IndexedTile]]
indexTile board horizontal = map (\row -> zip row [0 ..]) indexedBoard
  where
    indexedBoard =
      if not horizontal
        then board
        else transpose board

filterTile :: [[IndexedTile]] -> [[IndexedTile]]
filterTile = map (filter (\(tile, _) -> case tile of Empty -> False; _ -> True))

calculateDisplacement :: [[IndexedTile]] -> [[IndexedTile]] -> (Int, Int) -> [[TransitionTile]]
calculateDisplacement [] _ _ = []
calculateDisplacement (x : xs) (y : ys) arrows = calculateDisplacement' x y arrows 0 : calculateDisplacement xs ys arrows
calculateDisplacement _ _ _ = []

calculateDisplacement' :: [IndexedTile] -> [IndexedTile] -> (Int, Int) -> Int -> [TransitionTile]
calculateDisplacement' [] [] _ _ = []
calculateDisplacement' ((Empty, _) : xs) [] (_, _) acc = TransitionTileEmpty : calculateDisplacement' xs [] (0, 0) acc
calculateDisplacement' ((Tile x, i) : xs) ((Tile y, j) : ys) arrows@(arrX, arrY) acc
  | x == y || acc == 1 = transitionTile : calculateDisplacement' xs ys arrows 0
  | otherwise = transitionTile : calculateDisplacement' xs ((Tile y, j) : ys) arrows (acc + 1)
  where
    movX = if arrX == 0 then 0 else j - i
    movY = if arrY == 0 then 0 else j - i
    transitionTile = TransitionTile x (movX, movY)
calculateDisplacement' ((Empty, _) : xs) ty arrows acc = TransitionTileEmpty : calculateDisplacement' xs ty arrows acc
calculateDisplacement' _ _ _ _ = []

findNewTiles :: Board -> Board -> Board
findNewTiles = zipWith findNewTiles'

findNewTiles' :: [Tile] -> [Tile] -> [Tile]
findNewTiles' ((Tile x) : xs) ((Tile y) : ys)
  | x == y = Empty : findNewTiles' xs ys
  | otherwise = Tile y : findNewTiles' (tail xs) ys
findNewTiles' (Empty : xs) ((Tile y) : ys) = Tile y : findNewTiles' xs ys
findNewTiles' [] [] = []
findNewTiles' (Empty : xs) (Empty : ys) = Empty : findNewTiles' xs ys
findNewTiles' [] (Empty : ys) = Empty : findNewTiles' [] ys
findNewTiles' tile@(Tile _ : _) (Empty : ys) = Empty : findNewTiles' tile ys
findNewTiles' _ _ = []
