module Levels (Level (..), initializeLevels) where

import Data.Sequence (Seq)
import Maps (Coord, Item (..), predefinedItems, predefinedWalls)

data Level = Level
  { levelId :: Int,
    levelItems :: Seq Item,
    levelWalls :: Seq Coord,
    levelScoreRequired :: Int,
    levelHeight :: Int,
    levelWidth :: Int,
    levelTimeRequired :: Int
  }
  deriving (Show)

initializeLevels :: IO [Level]
initializeLevels = do
  level1Items <- predefinedItems 1
  level1Walls <- predefinedWalls 1
  level2Items <- predefinedItems 2
  level2Walls <- predefinedWalls 2
  level3Items <- predefinedItems 3
  level3Walls <- predefinedWalls 3
  level4Items <- predefinedItems 4
  level4Walls <- predefinedWalls 4
  level5Items <- predefinedItems 5
  level5Walls <- predefinedWalls 5
  return
    [ Level 1 level1Items level1Walls 5 10 10 10,
      Level 2 level2Items level2Walls 10 15 15 10,
      Level 3 level3Items level3Walls 15 15 15 10,
      Level 4 level4Items level4Walls 20 20 20 15,
      Level 5 level5Items level5Walls 25 20 20 15
    ]
