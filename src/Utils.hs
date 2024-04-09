module Utils (transpose) where

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([]:_) = []
transpose rows = map head rows : transpose (map tail rows)             
