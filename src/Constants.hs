module Constants
  ( height,
    width,
    ItemType (..),
    itemValue,
  )
where

height, width :: Int
height = 15
width = 15

data ItemType = Bronze | Silver | Gold | Pickable | Bomb deriving (Eq, Show)

itemValue :: ItemType -> Int
itemValue Bronze = 1
itemValue Silver = 2
itemValue Gold = 5
itemValue Pickable = 0
itemValue Bomb = -100