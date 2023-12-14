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
    emptyWidget,
    fg,
    hBox,
    hLimit,
    halt,
    neverShowCursor,
    on,
    padAll,
    padLeft,
    padRight,
    padTop,
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
    Game (_currentLevel, _initialTime, _inventory, _items, _walls),
    InventoryItem (itemName, itemQuantity),
    Item (itemCoord, itemType),
    ItemType (Bomb, Bronze, Gold, Pickable, Silver),
    Level (levelId, levelScoreRequired),
    dead,
    gamePassed,
    height,
    initialGoal,
    movePlayer,
    player,
    playerTrail,
    score,
    startGame,
    timeElapsed,
    width,
  )
import qualified Graphics.Vty as V
import Linear.V2 (V2 (..))

data Tick = Tick

data Cell = Player | ItemCell Item | Empty | Wall | PlayerTrail

type Name = ()

app :: App Game Tick Name
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
  g <- startGame
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  chan <- newBChan 10
  _ <- forkIO $
    forever $ do
      writeBChan chan Tick
      threadDelay 1000000
  void $ customMain initialVty builder (Just chan) app g

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (VtyEvent (V.EvKey (V.KChar 'y') [])) =
  if g ^. dead || g ^. gamePassed
    then restartGame
    else continue g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'n') [])) =
  if g ^. dead || g ^. gamePassed
    then halt g
    else continue g
handleEvent g (VtyEvent (V.EvKey V.KUp [])) =
  if not (g ^. dead || g ^. gamePassed)
    then continue $ movePlayer MUp g
    else continue g
handleEvent g (VtyEvent (V.EvKey V.KDown [])) =
  if not (g ^. dead || g ^. gamePassed)
    then continue $ movePlayer MDown g
    else continue g
handleEvent g (VtyEvent (V.EvKey V.KLeft [])) =
  if not (g ^. dead || g ^. gamePassed)
    then continue $ movePlayer MLeft g
    else continue g
handleEvent g (VtyEvent (V.EvKey V.KRight [])) =
  if not (g ^. dead || g ^. gamePassed)
    then continue $ movePlayer MRight g
    else continue g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) =
  if not (g ^. dead || g ^. gamePassed)
    then halt g
    else continue g
handleEvent g (VtyEvent (V.EvKey V.KEsc [])) =
  if not (g ^. dead || g ^. gamePassed)
    then halt g
    else continue g
handleEvent g (AppEvent Tick) =
  if g ^. dead || g ^. gamePassed
    then continue g
    else continue $ updateGame fixedDeltaTime g
  where
    fixedDeltaTime = 1.0
handleEvent g _ = continue g

restartGame :: EventM Name (Next Game)
restartGame = liftIO startGame >>= continue

updateGame :: Float -> Game -> Game
updateGame deltaTime g
  | g ^. gamePassed = g
  | g ^. timeElapsed <= 0 =
    let hasReachedGoal = g ^. score >= g ^. initialGoal
     in if hasReachedGoal
          then g & gamePassed .~ True
          else g & dead .~ True
  | otherwise =
    let newTime = max 0 (g ^. timeElapsed - round deltaTime)
        newGame = g & timeElapsed .~ newTime
        hasReachedGoal = g ^. score >= g ^. initialGoal
     in if hasReachedGoal
          then newGame & gamePassed .~ True
          else newGame

drawUI :: Game -> [Widget Name]
drawUI g =
  [ C.center $
      hBox
        [ padRight (Pad 2) $ drawGoal g,
          padRight (Pad 2) $ drawInventory (_inventory g),
          padRight (Pad 2) $ drawStats g,
          drawGrid g,
          -- padLeft (Pad 2) $ drawCurrentLevel g,
          padLeft (Pad 2) infoBox
        ]
  ]

drawInventory :: [InventoryItem] -> Widget n
drawInventory [] = emptyWidget
drawInventory (invItem : rest) =
  vBox
    [ str (show (itemName invItem) ++ ": " ++ show (itemQuantity invItem)),
      drawInventory rest
    ]

drawStats :: Game -> Widget Name
drawStats g
  | g ^. dead || g ^. gamePassed || g ^. timeElapsed <= 0 =
    vBox
      [ drawScore (g ^. score),
        padTop (Pad 2) $ drawStatus g,
        padTop (Pad 2) $ drawTimer g
      ]
  | otherwise =
    vBox
      [ drawScore (g ^. score),
        padTop (Pad 2) $ drawTimer g
      ]

drawScore :: Int -> Widget Name
drawScore n =
  hLimit 11 $
    withBorderStyle BS.unicodeBold $
      B.borderWithLabel (str "Score") $
        C.hCenter $
          padAll 1 $ str $ show n

drawTimer :: Game -> Widget Name
drawTimer g =
  hLimit 11 $
    withBorderStyle BS.unicodeBold $
      B.borderWithLabel (str "Time") $
        C.hCenter $
          padAll 1 $ str $ show (g ^. timeElapsed) ++ "s"

drawGoal :: Game -> Widget Name
drawGoal g =
  withBorderStyle BS.unicodeBold $
    B.borderWithLabel (str "Game Goal") $
      vBox
        [ padAll 1 $ str $ "Goal Points: " ++ show (levelScoreRequired $ _currentLevel g),
          padAll 1 $ str $ "Time Limit: " ++ show (_initialTime g) ++ "s",
          padAll 1 $ str $ "Current Level: " ++ show (levelId $ _currentLevel g)
        ]

drawGamePassed :: Widget Name
drawGamePassed =
  withAttr gamePassedAttr $
    vBox
      [ C.hCenter $ str "Game Passed! Congratulations!",
        str "Restart? (Y/N)"
      ]

drawStatus :: Game -> Widget Name
drawStatus g
  | g ^. gamePassed = drawGamePassed
  | otherwise = drawGameOver

drawGameOver :: Widget Name
drawGameOver =
  withAttr gameOverAttr $
    vBox
      [ C.hCenter $ str "Time's up! Game Over.",
        str "Restart? (Y/N)"
      ]

drawGrid :: Game -> Widget Name
drawGrid g =
  withBorderStyle BS.unicodeBold $
    B.borderWithLabel (str "Player") $
      vBox rows
  where
    rows = [hBox $ cellsInRow r | r <- [height -1, height -2 .. 0]]
    cellsInRow y = [drawCoord (V2 x y) | x <- [0 .. width -1]]
    drawCoord = drawCell . cellAt
    cellAt c
      | c `elem` g ^. player = Player
      | Just item <- find ((== c) . itemCoord) (_items g) = ItemCell item
      | c `elem` _walls g = Wall
      | c `elem` (g ^. playerTrail) = PlayerTrail
      | otherwise = Empty

drawCell :: Cell -> Widget Name
drawCell Player = withAttr playerAttr cw
drawCell PlayerTrail = withAttr playerTrailAttr cw
drawCell (ItemCell item) =
  case itemType item of
    Bronze -> withAttr bronzeAttr cw
    Silver -> withAttr silverAttr cw
    Gold -> withAttr goldAttr cw
    Pickable -> withAttr pickableAttr cw
    Bomb -> withAttr bombAttr cw
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
      (bombAttr, V.red `on` V.red)
    ]

gameOverAttr, gamePassedAttr :: AttrName
gameOverAttr = "gameOver"
gamePassedAttr = "gamePassed"

playerAttr, playerTrailAttr, emptyAttr, wallAttr, bronzeAttr, silverAttr, goldAttr, pickableAttr, bombAttr :: AttrName
playerAttr = "playerAttr"
playerTrailAttr = "playerTrailAttr"
emptyAttr = "emptyAttr"
wallAttr = "wallAttr"
bronzeAttr = "bronzeAttr"
silverAttr = "silverAttr"
goldAttr = "goldAttr"
pickableAttr = "pickableAttr"
bombAttr = "bombAttr"

infoBox :: Widget Name
infoBox =
  vLimit 20 $
    hLimit 50 $
      withBorderStyle BS.unicodeBold $
        B.borderWithLabel (str "Guidelines and Controls") $
          vBox
            [ str "Guidelines:",
              str "  Collect items to increase your score",
              str "  Please avoid walls and obstacles",
              str "\n",
              str "Controls:",
              str "  ↑: Move Up",
              str "  ↓: Move Down",
              str "  ←: Move Left",
              str "  →: Move Right",
              str "  'q' or 'Esc' to quit the game",
              str "\n",
              str "Item Values:",
              str "  Bronze: 1",
              str "  Silver: 2",
              str "  Gold:   5",
              str "  pickable: 0"
            ]
