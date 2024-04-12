module Constants (size, initialScore, twoPercentChance, tilesAmount) where

size :: Int
size = 4

initialScore :: Int
initialScore = 0

twoPercentChance :: Int
twoPercentChance = 6 -- 6/10 = 60% chance of getting a 2

tilesAmount :: Int
tilesAmount = size * size
