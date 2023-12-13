module Maps
  ( predefinedItems
  , predefinedWalls
  , Item(..)
  , Coord
  ) where

import Constants (ItemType(..))
import Data.Sequence (Seq(..))
import qualified Data.Sequence as S
import Linear.V2 (V2(..))

data Item = Item { itemType :: ItemType, itemCoord :: Coord } deriving (Eq, Show)

type Coord = V2 Int

predefinedItems :: Seq Item
predefinedItems = S.fromList [
    Item Bronze (V2 1 3), Item Silver (V2 2 5), Item Gold (V2 3 7),
    Item Bronze (V2 4 9), Item Silver (V2 5 11), Item Gold (V2 6 13),
    Item Bronze (V2 7 2), Item Silver (V2 8 4), Item Gold (V2 9 6),
    Item Bronze (V2 10 8), Item Silver (V2 11 10), Item Gold (V2 12 12),
    Item Silver (V2 13 14)
  ]

predefinedWalls :: Seq Coord
predefinedWalls = S.fromList [
    V2 0 0, V2 0 1, V2 0 2, V2 0 3, V2 0 4, V2 1 4, V2 2 4,
    V2 2 3, V2 2 2, V2 2 1, V2 3 1, V2 4 1, V2 5 1, V2 5 2,
    V2 5 3, V2 5 4, V2 6 4, V2 7 4, V2 7 1, V2 8 1, V2 9 1, 
    V2 10 1, V2 11 1, V2 11 2, V2 11 3, V2 11 4, V2 12 4, 
    V2 13 4, V2 14 4, V2 14 3, V2 14 2, V2 14 1, V2 14 0
  ]
