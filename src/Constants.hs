module Constants (size, initialScore, twoPercentChance,tilesAmount,initialAmountValue) where

size :: Int
size = 4

initialScore :: Int
initialScore = 0

twoPercentChance :: Int
twoPercentChance = 8 -- 8/10 = 80% chance of getting a 2

tilesAmount :: Int
tilesAmount = size*size

initialAmountValue :: Int
initialAmountValue = 2