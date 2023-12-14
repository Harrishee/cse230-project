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
    Game (_items, _walls),
    Item (itemCoord, itemType),
    ItemType (Bronze, Gold, Silver),
    dead,
    gamePassed,
    gameStarted,
    height,
    movePlayer,
    player,
    playerTrail,
    score,
    startGame,
    timeElapsed,
    width,
    initialGoal,
    initialTime,
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
  | otherwise = 
      let newTime = max 0 (g ^. timeElapsed - round deltaTime)
          newGame = g & timeElapsed .~ newTime
          hasReachedGoal = g ^. score >= g ^. initialGoal
          timeRemaining = newTime > 0
      in if hasReachedGoal && timeRemaining
           then newGame & gamePassed .~ True
           else newGame

drawUI :: Game -> [Widget Name]
drawUI g =
  [ C.center $
      hBox
        [ padRight (Pad 2) $ drawGoal g,
          padRight (Pad 2) $ drawStatsAndTimer g,
          drawGrid g,
          padLeft (Pad 2) $ infoBox
        ]
  ]

drawStatsAndTimer :: Game -> Widget Name
drawStatsAndTimer g =
  vBox
    [ drawStats g,
      padTop (Pad 2) $ drawTimer g
    ]

drawTimer :: Game -> Widget Name
drawTimer g =
  hLimit 11 $
    withBorderStyle BS.unicodeBold $
      B.borderWithLabel (str "Time") $
        C.hCenter $
          padAll 1 $
            str $ show (g ^. timeElapsed) ++ "s"

drawStats :: Game -> Widget Name
drawStats g =
  hLimit 11 $
    vBox
      [ drawScore (g ^. score),
        padTop (Pad 2) $ drawGameOver g
      ]

drawScore :: Int -> Widget Name
drawScore n =
  withBorderStyle BS.unicodeBold $
    B.borderWithLabel (str "Score") $
      C.hCenter $
        padAll 1 $
          str $ show n

drawGoal :: Game -> Widget Name
drawGoal g =
  withBorderStyle BS.unicodeBold $
    B.borderWithLabel (str "Game Goal") $
      vBox
        [ padAll 1 $ str $ "Goal Points: " ++ show (g ^. initialGoal),
          padAll 1 $ str $ "Time Limit: " ++ show (g ^. initialTime) ++ "s"
        ]

drawGameOver :: Game -> Widget Name
drawGameOver g =
  if g ^. dead || g ^. gamePassed
    then withAttr (if g ^. gamePassed then gamePassedAttr else gameOverAttr) $
      vBox [C.hCenter $ str (gameOverMessage g), str "  Restart?\n   (Y/N)"]
    else emptyWidget

gameOverMessage :: Game -> String
gameOverMessage g
  | g ^. gamePassed = "Game Passed"
  | otherwise       = "GAME OVER"

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
drawCell Empty = withAttr emptyAttr cw
drawCell Wall = withAttr wallAttr cw

cw :: Widget Name
cw = str "  "

theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [ (playerAttr, V.blue `on` V.blue),
      (playerTrailAttr, V.magenta `on` V.magenta),
      (gameOverAttr, fg V.red `V.withStyle` V.bold),
      (gamePassedAttr, fg V.green `V.withStyle` V.bold),
      (wallAttr, V.white `on` V.white),
      (bronzeAttr, V.red `on` V.red),
      (silverAttr, V.cyan `on` V.cyan),
      (goldAttr, V.yellow `on` V.yellow)
    ]

gameOverAttr, gamePassedAttr :: AttrName
gameOverAttr = "gameOver"
gamePassedAttr = "gamePassed"

playerAttr, playerTrailAttr, emptyAttr, wallAttr, bronzeAttr, silverAttr, goldAttr :: AttrName
playerAttr = "playerAttr"
playerTrailAttr = "playerTrailAttr"
emptyAttr = "emptyAttr"
wallAttr = "wallAttr"
bronzeAttr = "bronzeAttr"
silverAttr = "silverAttr"
goldAttr = "goldAttr"

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
              str "  Gold:   5"
            ]
