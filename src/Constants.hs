module Constants (size, initialScore, initialBestScore, twoPercentChance, tilesAmount, up, wKey, down, sKey, left, aKey, right, dKey) where

size :: Int
size = 4

initialScore :: Int
initialScore = 0

initialBestScore :: Int
initialBestScore = 0

twoPercentChance :: Int
twoPercentChance = 8

tilesAmount :: Int
tilesAmount = size * size

up :: Int
up = 38

wKey :: Int
wKey = 87

down :: Int
down = 40

sKey :: Int
sKey = 83

left :: Int
left = 37

aKey :: Int
aKey = 65

right :: Int
right = 39

dKey :: Int
dKey = 68
