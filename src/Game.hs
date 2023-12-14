{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Game
  ( startGame,
    turn,
    Game (..),
    Direction (..),
    ItemType (..),
    Item (..),
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
    initialGoal
  )
where

import Constants (ItemType (..), height, itemValue, width)
import Control.Lens (makeLenses, (%~), (&), (.~), (^.))
import Data.Foldable (find)
import Data.Sequence (Seq (..), ViewL ((:<)), viewl, (<|))
import qualified Data.Sequence as S
import Linear.V2 (V2 (..), _x, _y)
import Maps (Coord, Item (..), predefinedItems, predefinedWalls)

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
    _initialTime :: Int
  }
  deriving (Show)

type Player = Seq Coord

data Direction = MUp | MDown | MLeft | MRight deriving (Eq, Show)

makeLenses ''Game

movePlayer :: Direction -> Game -> Game
movePlayer d g =
  let movedG =
        if g ^. gameStarted
          then g & dir .~ d
          else g & dir .~ d & gameStarted .~ True
      nextHeadPos = nextPos movedG
      isWall = nextHeadPos `elem` (g ^. walls)
      isTrail = nextHeadPos `elem` (g ^. playerTrail)
   in if isWall || isTrail
        then g & dead .~ True
        else
          let newPlayerPos = nextHeadPos <| S.take (S.length (g ^. player) - 1) (g ^. player)
              newPlayerTrail = g ^. playerTrail S.|> S.index (g ^. player) 0
            in collectItem $ movedG & player .~ newPlayerPos & playerTrail .~ newPlayerTrail

collectItem :: Game -> Game
collectItem g =
  let curItems = g ^. items
      curPos = case viewl (g ^. player) of
        (a :< _) -> a
        _ -> error "Player sequence is empty"
   in case find ((== curPos) . itemCoord) curItems of
        Just item ->
          let scoreValue = itemValue $ itemType item
           in g & score %~ (+ scoreValue) & items %~ S.filter ((/= curPos) . itemCoord)
        Nothing -> g

nextPos :: Game -> Coord
nextPos Game {_dir = d, _player = (a :<| _)} =
  let newPos = case d of
        MUp -> a & _y %~ (\y -> min (y + 1) (height - 1))
        MDown -> a & _y %~ (\y -> max (y - 1) 0)
        MLeft -> a & _x %~ (\x -> max (x - 1) 0)
        MRight -> a & _x %~ (\x -> min (x + 1) (width - 1))
   in newPos
nextPos _ = error "Player can't be empty!"

turn :: Direction -> Game -> Game
turn d g =
  if g ^. dead
    then g
    else g & dir .~ d

startGame :: IO Game
startGame = do
  let xm = width `div` 2
  let ym = height `div` 2
  let g =
        Game
          { _player = S.singleton (V2 xm ym),
            _playerTrail = S.empty,
            _items = predefinedItems,
            _score = 0,
            _dir = MUp,
            _dead = False,
            _walls = predefinedWalls,
            _gameStarted = False,
            _timeElapsed = 20,
            _gamePassed = False,
            _initialGoal = 20,
            _initialTime = 20
          }
  return g
