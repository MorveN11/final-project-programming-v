{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}


module Transition (transition,initTransitionBoard,calculateDesplacement',  calculateDesplacement, updateTransitionTile,IndexedTile) where
import Constants (size)
import Utils (chop)
import Game (Board,TransitionTile (..), Tile (..),VisualBoard)

import Data.List (transpose)


type IndexedTile = (Tile ,Int)

initTransitionBoard :: Board -> [[TransitionTile]]
initTransitionBoard  = map ( map (\case
                                    Empty -> TransitionTileEmpty
                                    Tile t -> TransitionTile t (0, 0)))


transition :: Board -> Board -> (Int,Int) -> [[TransitionTile]]
transition prevBoard newBoard arrows@(arroX,_) | horizontal = transpose transitionedBoard
                                               |otherwise = transitionedBoard
                                        where
                                            horizontal = arroX == 0
                                            indexedPrevBoard = indexTile prevBoard horizontal
                                            indexedNewBoard =  filterTile (indexTile newBoard horizontal)
                                            transitionedBoard = calculateDesplacement indexedPrevBoard indexedNewBoard arrows


indexTile :: Board -> Bool -> [[IndexedTile]]
indexTile  board horizontal = map (\row -> zip row [0..]) indexedBoard
                            where indexedBoard = if not horizontal
                                                then board
                                                else transpose board


filterTile :: [[IndexedTile]]-> [[IndexedTile]]
filterTile  = map (filter (\(tile,_)-> case tile of Empty -> False ; _ -> True))


calculateDesplacement :: [[IndexedTile]]-> [[IndexedTile]] ->(Int,Int) ->[[TransitionTile]]
calculateDesplacement [x] [y] arrows  = [calculateDesplacement' x y arrows 0]
calculateDesplacement (x:xs) (y:ys) arrows  = calculateDesplacement' x y arrows 0: calculateDesplacement xs ys arrows


calculateDesplacement' :: [IndexedTile] -> [IndexedTile] -> (Int,Int) -> Int -> [TransitionTile]
calculateDesplacement' [] [] _ _ = []
calculateDesplacement' ((Empty,_):xs) [] (_,_) acc= TransitionTileEmpty : calculateDesplacement' xs [] (0,0) acc
calculateDesplacement' ((Tile x,i):xs) ((Tile y,j):ys) arrows@(arrX,arrY) acc
                        | x == y  || acc == 1 =  transitionTile : calculateDesplacement' xs ys arrows 0
                        | otherwise = transitionTile : calculateDesplacement' xs ((Tile y,j):ys) arrows (acc+1)
                        where
                            movX = if arrX == 0 then 0 else j-i
                            movY = if arrY == 0 then 0 else j-i
                            transitionTile = TransitionTile x (movX,movY)
calculateDesplacement' ((Empty,_):xs) ty arrows acc = TransitionTileEmpty : calculateDesplacement' xs ty arrows acc



updateTransitionTile :: VisualBoard -> Int -> Int -> VisualBoard
updateTransitionTile grid index value =
  chop size (xs ++ [TransitionTile value (0,0)] ++ tail ys)
  where
    (xs, ys) = splitAt index (concat grid)

