{-# LANGUAGE OverloadedStrings #-}

module UI (drawGame) where

import Brick
  ( App (..),
    AttrMap,
    AttrName,
    BrickEvent (..),
    EventM,
    Next,
    Padding (..),
    Widget,
    attrMap,
    continue,
    customMain,
    fg,
    hBox,
    hLimit,
    halt,
    neverShowCursor,
    on,
    padAll,
    padLeft,
    padLeftRight,
    padRight,
    str,
    vBox,
    vLimit,
    withAttr,
    withBorderStyle,
  )
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Control.Concurrent (forkIO, threadDelay)
import Control.Lens ((&), (.~), (^.))
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (find)
import Game
  ( Direction (MDown, MLeft, MRight, MUp),
    Game (_currentLevel, _inventory, _items, _walls),
    InventoryItem (itemQuantity),
    Item (itemCoord, itemType),
    ItemType (Bomb, Bronze, Gold, Pickable, Silver),
    dead,
    gamePassed,
    initialGoal,
    movePlayer,
    player,
    playerTrail,
    score,
    startGame,
    timeElapsed,
  )
import qualified Graphics.Vty as V
import Levels (Level (..), levelHeight, levelWidth)
import Linear.V2 (V2 (..))

data Tick = Tick

data Cell = Player | ItemCell Game.Item | Empty | Wall | PlayerTrail

type Name = ()

app :: App Game.Game Tick Name
app =
  App
    { appDraw = drawUI,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent,
      appStartEvent = return,
      appAttrMap = const theMap
    }

drawGame :: IO ()
drawGame = do
  g <- Game.startGame
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  chan <- newBChan 10
  _ <- forkIO $
    forever $ do
      writeBChan chan Tick
      threadDelay 1000000
  void $ customMain initialVty builder (Just chan) app g

handleEvent :: Game.Game -> BrickEvent Name Tick -> EventM Name (Next Game.Game)
handleEvent g (VtyEvent (V.EvKey (V.KChar 'y') [])) =
  if g ^. Game.dead || g ^. Game.gamePassed
    then restartGame
    else continue g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'n') [])) =
  if g ^. Game.dead || g ^. Game.gamePassed
    then halt g
    else continue g
handleEvent g (VtyEvent (V.EvKey V.KUp [])) =
  if not (g ^. Game.dead || g ^. Game.gamePassed)
    then continue $ Game.movePlayer Game.MUp g
    else continue g
handleEvent g (VtyEvent (V.EvKey V.KDown [])) =
  if not (g ^. Game.dead || g ^. Game.gamePassed)
    then continue $ Game.movePlayer Game.MDown g
    else continue g
handleEvent g (VtyEvent (V.EvKey V.KLeft [])) =
  if not (g ^. Game.dead || g ^. Game.gamePassed)
    then continue $ Game.movePlayer Game.MLeft g
    else continue g
handleEvent g (VtyEvent (V.EvKey V.KRight [])) =
  if not (g ^. Game.dead || g ^. Game.gamePassed)
    then continue $ Game.movePlayer Game.MRight g
    else continue g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) =
  if not (g ^. Game.dead || g ^. Game.gamePassed)
    then halt g
    else continue g
handleEvent g (VtyEvent (V.EvKey V.KEsc [])) =
  if not (g ^. Game.dead || g ^. Game.gamePassed)
    then halt g
    else continue g
handleEvent g (AppEvent Tick) =
  if g ^. Game.dead || g ^. Game.gamePassed
    then continue g
    else continue $ updateGame fixedDeltaTime g
  where
    fixedDeltaTime = 1.0
handleEvent g _ = continue g

restartGame :: EventM Name (Next Game.Game)
restartGame = liftIO Game.startGame >>= continue

updateGame :: Float -> Game.Game -> Game.Game
updateGame deltaTime g
  | g ^. Game.gamePassed = g
  | g ^. Game.timeElapsed <= 0 =
    let hasReachedGoal = g ^. Game.score >= g ^. Game.initialGoal
     in if hasReachedGoal
          then g & Game.gamePassed .~ True
          else g & Game.dead .~ True
  | otherwise =
    let newTime = max 0 (g ^. Game.timeElapsed - round deltaTime)
        newGame = g & Game.timeElapsed .~ newTime
        hasReachedGoal = g ^. Game.score >= g ^. Game.initialGoal
     in if hasReachedGoal
          then newGame & Game.gamePassed .~ True
          else newGame

drawUI :: Game.Game -> [Widget Name]
drawUI g =
  [ C.center $
      if g ^. Game.dead || g ^. Game.gamePassed
        then drawStatus g
        else
          hBox
            [ padRight (Pad 2) $ drawInventory (Game._inventory g),
              padRight (Pad 3) $ drawGoal g,
              drawGrid g,
              padLeft (Pad 3) infoBox
            ]
  ]

drawInventory :: [Game.InventoryItem] -> Widget n
drawInventory inv =
  withBorderStyle BS.unicodeBold $
    B.borderWithLabel (str "Inventory") $
      vBox
        [ padLeftRight 1 $ padAll 1 $ withAttr bronzeAttr $ str $ "Bronze: " ++ showQuantity 0,
          padLeftRight 1 $ padAll 1 $ withAttr silverAttr $ str $ "Silver: " ++ showQuantity 1,
          padLeftRight 1 $ padAll 1 $ withAttr goldAttr $ str $ "Gold: " ++ showQuantity 2,
          padLeftRight 1 $ padAll 1 $ withAttr pickableAttr $ str $ "Pickable: " ++ showQuantity 3,
          padLeftRight 1 $ padAll 1 $ withAttr bombAttr $ str $ "Bomb: " ++ showQuantity 4
        ]
  where
    showQuantity n = maybe "0" (show . Game.itemQuantity) (safeGet n inv)

safeGet :: Int -> [a] -> Maybe a
safeGet n xs = if n < length xs then Just (xs !! n) else Nothing

drawGoal :: Game.Game -> Widget Name
drawGoal g =
  withBorderStyle BS.unicodeBold $
    B.borderWithLabel (str "Game Goal") $
      vBox
        [ padLeftRight 2 $ padAll 1 $ withAttr levelAttr $ str $ "Current Level: " ++ show (levelId $ Game._currentLevel g),
          padLeftRight 2 $ padAll 1 $ withAttr gamePassedAttr $ str $ "Goal Score: " ++ show (levelScoreRequired $ Game._currentLevel g),
          padLeftRight 2 $ padAll 1 $ str $ "Your Score: " ++ show (g ^. Game.score),
          padLeftRight 2 $ padAll 1 $ withAttr gamePassedAttr $ str $ "Time Limit: " ++ show (levelTimeRequired $ Game._currentLevel g) ++ "s",
          padLeftRight 2 $ padAll 1 $ str $ "Time Left: " ++ show (g ^. Game.timeElapsed) ++ "s"
        ]

drawGamePassed :: Widget Name
drawGamePassed =
  withAttr gamePassedAttr $
    vBox
      [ C.hCenter $ str "Game Passed! Congratulations!",
        C.hCenter $ str "Restart? (Y/N)"
      ]

drawStatus :: Game.Game -> Widget Name
drawStatus g
  | g ^. Game.gamePassed = drawGamePassed
  | otherwise = drawGameOver

drawGameOver :: Widget Name
drawGameOver =
  withAttr gameOverAttr $
    vBox
      [ C.hCenter $ str "Time's up! Game Over.",
        C.hCenter $ str "Restart? (Y/N)"
      ]

drawGrid :: Game.Game -> Widget Name
drawGrid g =
  withBorderStyle BS.unicodeBold $
    B.borderWithLabel (str "Player") $
      vBox rows
  where
    currentLevel = Game._currentLevel g
    currentLevelHeight = levelHeight currentLevel
    currentLevelWidth = levelWidth currentLevel
    rows = [hBox $ cellsInRow r | r <- [currentLevelHeight - 1, currentLevelHeight - 2 .. 0]]
    cellsInRow y = [drawCoord (V2 x y) | x <- [0 .. currentLevelWidth - 1]]
    drawCoord = drawCell . cellAt
    cellAt c
      | c `elem` g ^. Game.player = Player
      | Just item <- find ((== c) . Game.itemCoord) (Game._items g) = ItemCell item
      | c `elem` Game._walls g = Wall
      | c `elem` (g ^. Game.playerTrail) = PlayerTrail
      | otherwise = Empty

drawCell :: Cell -> Widget Name
drawCell Player = withAttr playerAttr cw
drawCell PlayerTrail = withAttr playerTrailAttr cw
drawCell (ItemCell item) =
  case Game.itemType item of
    Game.Bronze -> withAttr bronzeAttr (str "B ")
    Game.Silver -> withAttr silverAttr (str "S ")
    Game.Gold -> withAttr goldAttr (str "G ")
    Game.Pickable -> withAttr pickableAttr cw
    Game.Bomb -> withAttr bombAttr cw
drawCell Empty = withAttr emptyAttr cw
drawCell Wall = withAttr wallAttr cw

cw :: Widget Name
cw = str "  "

theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [ (playerAttr, V.blue `on` V.blue),
      (playerTrailAttr, V.white `on` V.white),
      (gameOverAttr, fg V.red `V.withStyle` V.bold),
      (gamePassedAttr, fg V.green `V.withStyle` V.bold),
      (wallAttr, V.white `on` V.white),
      (bronzeAttr, V.magenta `on` V.magenta),
      (silverAttr, V.cyan `on` V.cyan),
      (goldAttr, V.yellow `on` V.yellow),
      (pickableAttr, V.green `on` V.green),
      (bombAttr, V.red `on` V.red),
      (levelAttr, fg V.blue),
      (whiteTextAttr, fg V.white)
    ]

playerAttr, playerTrailAttr, emptyAttr, wallAttr, bronzeAttr, silverAttr, goldAttr, pickableAttr, bombAttr, levelAttr, gameOverAttr, gamePassedAttr, whiteTextAttr :: AttrName
playerAttr = "playerAttr"
playerTrailAttr = "playerTrailAttr"
emptyAttr = "emptyAttr"
wallAttr = "wallAttr"
bronzeAttr = "bronzeAttr"
silverAttr = "silverAttr"
goldAttr = "goldAttr"
pickableAttr = "pickableAttr"
bombAttr = "bombAttr"
levelAttr = "levelAttr"
gameOverAttr = "gameOver"
gamePassedAttr = "gamePassed"
whiteTextAttr = "whiteTextAttr"

infoBox :: Widget Name
infoBox =
  vLimit 20 $
    hLimit 50 $
      withBorderStyle BS.unicodeBold $
        B.borderWithLabel (str "Guidelines and Controls") $
          vBox
            [ str "Guidelines:",
              str "  Collect items to increase your score",
              str "  Please avoid walls and bombs",
              str "\n",
              str "Controls:",
              str "  ↑: Move Up",
              str "  ↓: Move Down",
              str "  ←: Move Left",
              str "  →: Move Right",
              str "  q: to quit in the game",
              str "\n",
              str "Item Values:",
              str "  Bronze: 1",
              str "  Silver: 2",
              str "  Gold:   5",
              str "  pickable: 0"
            ]
