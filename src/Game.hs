{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Game
  ( startGame,
    Game (..),
    Direction (..),
    ItemType (..),
    Item (..),
    InventoryItem (..),
    Level (..),
    dead,
    score,
    player,
    playerTrail,
    height,
    width,
    movePlayer,
    gameStarted,
    timeElapsed,
    gamePassed,
    initialTime,
    initialGoal,
    initializeLevels,
  )
where

import Constants (ItemType (..), height, itemValue, width)
import Control.Lens (makeLenses, (%~), (&), (.~), (^.))
import Data.Foldable (find)
import Data.Sequence (Seq (..), (<|))
import qualified Data.Sequence as S
import Levels (Level (..), initializeLevels, levelId, levelItems, levelScoreRequired, levelWalls)
import Linear.V2 (V2 (..), _x, _y)
import Maps (Coord, Item (..))

data Game = Game
  { _player :: Player,
    _playerTrail :: Seq Coord,
    _items :: Seq Item,
    _walls :: Seq Coord,
    _score :: Int,
    _dir :: Direction,
    _dead :: Bool,
    _timeElapsed :: Int,
    _gameStarted :: Bool,
    _gamePassed :: Bool,
    _initialGoal :: Int,
    _initialTime :: Int,
    _inventory :: [InventoryItem],
    _currentLevel :: Level,
    _levels :: [Level]
  }
  deriving (Show)

type Player = Seq Coord

data InventoryItem = InventoryItem
  { itemName :: ItemType,
    itemQuantity :: Int
  }
  deriving (Show)

data Direction = MUp | MDown | MLeft | MRight deriving (Eq, Show)

makeLenses ''Game

movePlayer :: Direction -> Game -> Game
movePlayer d g =
  let movedG =
        if g ^. gameStarted
          then g & dir .~ d
          else g & dir .~ d & gameStarted .~ True
      nextHeadPos = nextPos movedG
      isBomb = isNextPositionBomb movedG
      hasPickable = hasPickableItem g
      isWall = nextHeadPos `elem` (g ^. walls)
      isTrail = nextHeadPos `elem` (g ^. playerTrail)
   in if isBomb
        then g & dead .~ True
        else if (isWall || isTrail) && not hasPickable
          then g
          else if hasPickable
            then
              let newPlayerPos = nextHeadPos <| S.take (S.length (g ^. player) - 1) (g ^. player)
                  newPlayerTrail = g ^. playerTrail S.|> S.index (g ^. player) 0
                  updatedGame =
                    movedG
                      & player .~ newPlayerPos
                      & playerTrail .~ newPlayerTrail
                      & inventory %~ consumePickableItem
               in checkLevelCompletion $ pickUpItem updatedGame nextHeadPos
            else
              let newPlayerPos = nextHeadPos <| S.take (S.length (g ^. player) - 1) (g ^. player)
                  newPlayerTrail = g ^. playerTrail S.|> S.index (g ^. player) 0
                  updatedGame = movedG & player .~ newPlayerPos & playerTrail .~ newPlayerTrail
               in checkLevelCompletion $ pickUpItem updatedGame nextHeadPos

consumePickableItem :: [InventoryItem] -> [InventoryItem]
consumePickableItem [] = []
consumePickableItem (item : rest)
  | itemName item == Pickable && itemQuantity item > 0 = item { itemQuantity = itemQuantity item - 1 } : rest
  | otherwise = item : consumePickableItem rest

hasPickableItem :: Game -> Bool
hasPickableItem game =
  case find (\item -> itemName item == Pickable && itemQuantity item > 0) (_inventory game) of
    Just _ -> True
    Nothing -> False

isNextPositionBomb :: Game -> Bool
isNextPositionBomb game =
  let nextHeadPos = nextPos game
      itemsAtNextPos = findItemsAtCoord nextHeadPos (_items game)
  in any (\item -> itemType item == Bomb) itemsAtNextPos

findItemsAtCoord :: Coord -> Seq Item -> Seq Item
findItemsAtCoord coord items = S.filter (\item -> itemCoord item == coord) items

pickUpItem :: Game -> Coord -> Game
pickUpItem game coord =
  case findItem coord (_items game) of
    Just item ->
      let itemsName = itemType item
          updatedInventory = addItemToInventory itemsName (_inventory game)
          scoreValue = itemValue itemsName
       in checkLevelCompletion $
            game & items %~ S.filter ((/= coord) . itemCoord)
              & inventory .~ updatedInventory
              & score %~ (+ scoreValue)
    Nothing -> game

findItem :: Coord -> Seq Item -> Maybe Item
findItem coord = find ((== coord) . itemCoord)

addItemToInventory :: ItemType -> [InventoryItem] -> [InventoryItem]
addItemToInventory newItemName [] = [InventoryItem newItemName 1]
addItemToInventory newItemName (item : rest)
  | newItemName == itemName item = (item {itemQuantity = itemQuantity item + 1}) : rest
  | otherwise = item : addItemToInventory newItemName rest

nextPos :: Game -> Coord
nextPos Game {_dir = d, _player = (a :<| _)} =
  let newPos = case d of
        MUp -> a & _y %~ (\y -> min (y + 1) (height - 1))
        MDown -> a & _y %~ (\y -> max (y - 1) 0)
        MLeft -> a & _x %~ (\x -> max (x - 1) 0)
        MRight -> a & _x %~ (\x -> min (x + 1) (width - 1))
   in newPos
nextPos _ = error "Player can't be empty!"

checkLevelCompletion :: Game -> Game
checkLevelCompletion game =
  if _score game >= levelScoreRequired (_currentLevel game)
    then moveToNextLevel game
    else game

resetGameStateForNewLevel :: Level -> Game -> Game
resetGameStateForNewLevel level game =
  game
    { _player = S.singleton initialPlayerPosition,
      _playerTrail = S.empty,
      _items = levelItems level,
      _walls = levelWalls level,
      _score = 0,
      _dir = MUp,
      _dead = False,
      _gameStarted = False,
      _timeElapsed = _initialTime game,
      _currentLevel = level,
      _inventory = [InventoryItem Bronze 0, InventoryItem Silver 0, InventoryItem Gold 0, InventoryItem Pickable 0, InventoryItem Bomb 0]
    }
  where
    initialPlayerPosition = V2 (width `div` 2) (height `div` 2)


moveToNextLevel :: Game -> Game
moveToNextLevel game =
  let currentLevelId = levelId $ _currentLevel game
      nextLevelId = currentLevelId + 1
      nextLevel = find (\level -> levelId level == nextLevelId) (_levels game)
   in case nextLevel of
        Just level -> resetGameStateForNewLevel level game
        Nothing -> game

startGame :: IO Game
startGame = do
  let lv = initializeLevels
  let initialLevel = head lv
  let xm = width `div` 2
  let ym = height `div` 2
  let initialInventory = [InventoryItem Bronze 0, InventoryItem Silver 0, InventoryItem Gold 0, InventoryItem Pickable 0, InventoryItem Bomb 0]
  let g =
        Game
          { _player = S.singleton (V2 xm ym),
            _playerTrail = S.empty,
            _items = levelItems initialLevel,
            _score = 0,
            _dir = MUp,
            _dead = False,
            _walls = levelWalls initialLevel,
            _gameStarted = False,
            _timeElapsed = 20,
            _gamePassed = False,
            _initialGoal = 20,
            _initialTime = 20,
            _inventory = initialInventory,
            _currentLevel = initialLevel,
            _levels = lv
          }
  return g
