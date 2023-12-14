module Constants
  ( 
    ItemType (..),
    itemValue,
  )
where

data ItemType = Bronze | Silver | Gold | Bomb | WallBreaker | Teleport deriving (Eq, Show)

itemValue :: ItemType -> Int
itemValue Bronze = 1
itemValue Silver = 2
itemValue Gold = 5
itemValue WallBreaker = 0
itemValue Teleport = 0
itemValue Bomb = -100