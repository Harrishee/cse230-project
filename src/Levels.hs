module Levels (Level (..), initializeLevels) where

import Data.Sequence (Seq)
import Maps (Coord, Item (..), predefinedItems, predefinedWalls)

data Level = Level
  { levelId :: Int,
    levelItems :: Seq Item,
    levelWalls :: Seq Coord,
    levelScoreRequired :: Int
  }
  deriving (Show)

initializeLevels :: [Level]
initializeLevels =
  [ Level 1 (predefinedItems 1) (predefinedWalls 1) 5,
    Level 2 (predefinedItems 2) (predefinedWalls 2) 10,
    Level 3 (predefinedItems 3) (predefinedWalls 3) 15,
    Level 4 (predefinedItems 4) (predefinedWalls 4) 20,
    Level 5 (predefinedItems 5) (predefinedWalls 5) 25
  ]
