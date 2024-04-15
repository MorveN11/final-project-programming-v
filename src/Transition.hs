{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}

module Transition (transition,initTransitionBoard,calculateDesplacement',  calculateDesplacement, updateTransitionTile,IndexedTile) where
import Constants (size)
import Utils (chop)
import Game (Board,TransitionTile (..), Tile (..),VisualBoard(..))

import Data.List (transpose)


type IndexedTile = (Tile ,Int, Int)

initTransitionBoard :: Board -> [[TransitionTile]]
initTransitionBoard  = map ( map (\ tile
                                    -> case tile of
                                            Empty -> TransitionTileEmpty
                                            Tile t -> TransitionTile t (0,0)))


transition :: Board -> Board -> (Int,Int) -> [[TransitionTile]]
transition prevBoard newBoard arrows@(arroX,_) | horizontal = transpose (calculateDesplacement indexedPrevBoard indexedNewBoard arrows)
                                                   |otherwise = calculateDesplacement indexedPrevBoard indexedNewBoard arrows
                                        where
                                            horizontal = arroX == 0
                                            indexedPrevBoard = indexTile prevBoard horizontal
                                            indexedNewBoard =  filterTile (indexTile newBoard horizontal)


indexTile :: Board -> Bool -> [[IndexedTile]]
indexTile  board horizontal = map (\row -> map (\(t, n) -> (t,n,0))  (zip row [0..])) indexedBoard
                            where indexedBoard = if not horizontal
                                                then board
                                                else transpose board


filterTile :: [[IndexedTile]]-> [[IndexedTile]]
filterTile  = map (filter (\(tile,_,_)-> case tile of Empty -> False ; _ -> True))


calculateDesplacement :: [[IndexedTile]]-> [[IndexedTile]] ->(Int,Int) ->[[TransitionTile]]
calculateDesplacement [x] [y] arrows  = [calculateDesplacement' x y arrows]
calculateDesplacement (x:xs) (y:ys) arrows  = calculateDesplacement' x y arrows : calculateDesplacement xs ys arrows


calculateDesplacement' :: [IndexedTile] -> [IndexedTile] ->(Int,Int) ->[TransitionTile]
calculateDesplacement' [] [] _  = []
calculateDesplacement' ((Empty,_,_):xs) [] (_,_) = TransitionTileEmpty : calculateDesplacement' xs [] (0,0)
calculateDesplacement' ((Tile x,i,_):xs) ((Tile y,j,acc):ys) arrows@(arrX,arrY)
                        | x == y  || acc == 1 =  transitionTile : calculateDesplacement' xs ys arrows
                    
                        | otherwise = transitionTile : calculateDesplacement' xs ((Tile y,j,acc+1):ys) arrows
                        where
                            movX = if arrX == 0 then 0 else j-i
                            movY = if arrY == 0 then 0 else j-i
                            transitionTile = TransitionTile x (movX,movY)
calculateDesplacement' ((Empty,_,_):xs) ty arrows = TransitionTileEmpty : calculateDesplacement' xs ty arrows




updateTransitionTile :: VisualBoard -> Int -> Int -> VisualBoard
updateTransitionTile grid index value =
  chop size (xs ++ [TransitionTile value (0,0)] ++ tail ys)
  where
    (xs, ys) = splitAt index (concat grid)

