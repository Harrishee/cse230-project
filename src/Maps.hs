module Maps
  ( predefinedItems
  , predefinedWalls
  , Item(..)
  , Coord
  ) where

import System.Random (randomRIO)
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Linear.V2 (V2(..))
import Constants (ItemType(..))

type Coord = V2 Int
data Item = Item { itemType :: ItemType, itemCoord :: Coord } deriving (Eq, Show)

type LevelMaps = [([Coord], [Item])]

predefinedMaps :: [LevelMaps]
predefinedMaps =
    [
        [ -- Level 1 10x10 5pt
            ([V2 1 4, V2 2 4, V2 4 3], [Item WallBreaker (V2 1 3), Item Silver (V2 2 5), Item Gold (V2 3 7)]),
            ([V2 3 4, V2 4 4, V2 9 3], [Item Bronze (V2 4 9), Item Silver (V2 5 9), Item Gold (V2 6 3)]),
            ([V2 3 9, V2 4 8, V2 9 3], [Item Bronze (V2 0 1), Item Bomb (V2 5 0), Item Gold (V2 6 2)]),
            ([V2 3 3, V2 4 4, V2 5 5], [Item Bronze (V2 4 9), Item Silver (V2 2 3), Item Gold (V2 6 6)]),
            ([V2 0 0, V2 4 4, V2 1 1], [Item Bronze (V2 6 6), Item Silver (V2 2 3), Item Gold (V2 2 2)])
            -- Add more sets for level 1
        ],
        [ -- Level 2 15x15 10pt
            ([V2 1 14, V2 2 14, V2 14 13, V2 9 2], [Item WallBreaker (V2 1 3), Item Silver (V2 2 5), Item Gold (V2 3 7), Item Silver (V2 12 5), Item Gold (V2 13 7)]),
            ([V2 13 14, V2 14 4, V2 5 3, V2 12 3], [Item Bronze (V2 14 9), Item Silver (V2 14 18), Item Gold (V2 6 3), Item Silver (V2 3 5), Item Gold (V2 13 7)]),
            ([V2 3 12, V2 8 8, V2 9 8, V2 9 9], [Item Bronze (V2 0 1), Item Bomb (V2 5 0), Item Gold (V2 6 2), Item Silver (V2 12 9), Item Gold (V2 13 7)]),
            ([V2 3 13, V2 2 4, V2 5 5, V2 3 3], [Item Bronze (V2 4 9), Item Silver (V2 2 3), Item Gold (V2 6 6), Item Silver (V2 2 8), Item Gold (V2 13 5)]),
            ([V2 0 0, V2 14 4, V2 13 1, V2 14 3], [Item Bronze (V2 6 6), Item Silver (V2 2 3), Item Gold (V2 2 2), Item Silver (V2 0 5), Item Gold (V2 2 7)])
        ],
        [ -- Level 3 15x15 15pt
            ([V2 1 14, V2 2 14, V2 14 13, V2 9 2, V2 9 12], [Item Bronze (V2 1 3), Item Silver (V2 2 5), Item Gold (V2 3 7), Item Silver (V2 12 5), Item Gold (V2 13 7), Item Silver (V2 12 14), Item Gold (V2 13 14)]),
            ([V2 13 14, V2 14 4, V2 5 3, V2 12 3, V2 7 14], [Item Bronze (V2 14 9), Item Silver (V2 14 18), Item Gold (V2 6 3), Item Silver (V2 3 5), Item Gold (V2 13 7), Item Silver (V2 12 1), Item Gold (V2 13 5)]),
            ([V2 3 12, V2 8 8, V2 9 8, V2 9 9, V2 8 2], [Item Bronze (V2 0 1), Item Bomb (V2 5 0), Item Gold (V2 6 2), Item WallBreaker (V2 12 9), Item Gold (V2 13 7), Item Silver (V2 7 5), Item Gold (V2 13 6)]),
            ([V2 3 13, V2 2 4, V2 5 5, V2 3 3, V2 6 1], [Item Bronze (V2 4 9), Item Silver (V2 2 3), Item Gold (V2 6 6), Item Silver (V2 2 8), Item Gold (V2 13 5), Item Silver (V2 11 5), Item Gold (V2 12 4)]),
            ([V2 0 0, V2 14 4, V2 13 1, V2 14 3, V2 6 2], [Item Bronze (V2 6 6), Item Silver (V2 2 3), Item Gold (V2 2 2), Item Silver (V2 0 5), Item Gold (V2 2 7), Item Silver (V2 7 5), Item Gold (V2 9 8)])
        ],
        [ -- Level 4 20x20 20pt
            ([V2 1 19, V2 12 17, V2 4 13, V2 9 2, V2 9 12], [Item Bronze (V2 1 3), Item Silver (V2 2 5), Item Gold (V2 3 7), Item Silver (V2 12 5), Item Gold (V2 13 7), Item Silver (V2 12 14), Item Gold (V2 13 14), Item Silver (V2 12 17), Item Gold (V2 13 18)]),
            ([V2 13 14, V2 19 4, V2 5 13, V2 12 3, V2 7 19], [Item Bronze (V2 14 9), Item Bomb (V2 14 18), Item Gold (V2 6 3), Item Silver (V2 3 5), Item Gold (V2 13 7), Item Silver (V2 12 1), Item Gold (V2 13 5), Item Silver (V2 2 19), Item Gold (V2 6 18)]),
            ([V2 3 12, V2 8 18, V2 9 8, V2 9 19, V2 8 2], [Item Bronze (V2 0 1), Item Silver (V2 5 0), Item Gold (V2 6 2), Item Silver (V2 12 9), Item Gold (V2 13 7), Item WallBreaker (V2 7 5), Item Gold (V2 13 6), Item Silver (V2 12 4), Item Gold (V2 13 16)]),
            ([V2 3 13, V2 2 4, V2 5 5, V2 3 13, V2 6 1], [Item Bronze (V2 4 9), Item Silver (V2 2 3), Item Bomb (V2 6 6), Item Silver (V2 2 8), Item Gold (V2 13 5), Item Silver (V2 11 5), Item Gold (V2 12 4), Item Silver (V2 13 17), Item Gold (V2 13 18)]),
            ([V2 0 0, V2 14 4, V2 13 16, V2 14 13, V2 16 2], [Item Bronze (V2 6 6), Item Silver (V2 2 3), Item Gold (V2 2 2), Item Silver (V2 0 5), Item WallBreaker (V2 2 7), Item Silver (V2 7 5), Item Gold (V2 9 8), Item Silver (V2 12 11), Item Gold (V2 11 15)])
        ],
        [ -- Level 5 20x20 25pt
            ([V2 1 19, V2 12 17, V2 14 13, V2 9 2, V2 9 12], [Item Bronze (V2 1 3), Item Silver (V2 2 5), Item WallBreaker (V2 3 7), Item Silver (V2 12 5), Item Gold (V2 13 7), Item Silver (V2 12 14), Item Gold (V2 13 14), Item Silver (V2 12 17), Item Gold (V2 13 18), Item Bronze (V2 18 13), Item Silver (V2 12 5), Item Gold (V2 16 7)]),
            ([V2 13 14, V2 19 4, V2 8 13, V2 12 13, V2 7 19], [Item Bronze (V2 14 9), Item Silver (V2 14 18), Item Gold (V2 6 3), Item Silver (V2 3 5), Item Gold (V2 13 7), Item Silver (V2 12 1), Item Gold (V2 13 5), Item Silver (V2 2 19), Item Gold (V2 6 18), Item Bronze (V2 12 3), Item Silver (V2 12 5), Item Gold (V2 3 19)]),
            ([V2 3 12, V2 8 18, V2 9 18, V2 9 19, V2 8 2], [Item Bronze (V2 0 1), Item Bomb (V2 5 0), Item Gold (V2 6 2), Item Silver (V2 12 9), Item Gold (V2 13 7), Item Silver (V2 7 5), Item Gold (V2 13 6), Item Silver (V2 12 4), Item Gold (V2 13 16), Item Bronze (V2 12 3), Item Silver (V2 12 18), Item Gold (V2 13 10)]),
            ([V2 3 19, V2 2 4, V2 5 5, V2 3 13, V2 16 15], [Item Bronze (V2 4 9), Item Silver (V2 2 3), Item Gold (V2 6 6), Item Bomb (V2 2 8), Item Gold (V2 13 5), Item Silver (V2 11 5), Item Gold (V2 12 4), Item Bomb (V2 13 17), Item Gold (V2 13 18), Item Bronze (V2 11 3), Item Silver (V2 12 10), Item Gold (V2 0 17)]),
            ([V2 0 10, V2 14 4, V2 13 16, V2 14 13, V2 16 12], [Item WallBreaker (V2 6 6), Item Silver (V2 2 3), Item Gold (V2 2 2), Item Silver (V2 0 5), Item Gold (V2 2 7), Item Silver (V2 7 5), Item Gold (V2 9 8), Item Silver (V2 12 11), Item Gold (V2 11 15), Item Bronze (V2 11 13), Item Silver (V2 12 9), Item Gold (V2 17 16)])
        ]
    ]

getRandomMap :: Int -> IO (Seq Coord, Seq Item)
getRandomMap levelId = do
    let levelMaps = predefinedMaps !! (levelId - 1)
    idx <- randomRIO (0, length levelMaps - 1)
    let (walls, items) = levelMaps !! idx
    return (S.fromList walls, S.fromList items)

predefinedWalls :: Int -> IO (Seq Coord)
predefinedWalls levelId = fst <$> getRandomMap levelId

predefinedItems :: Int -> IO (Seq Item)
predefinedItems levelId = snd <$> getRandomMap levelId
