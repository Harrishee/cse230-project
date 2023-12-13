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
    score,
    startGame,
    timeElapsed,
    width,
  )
import qualified Graphics.Vty as V
import Linear.V2 (V2 (..))

data Tick = Tick

data Cell = Player | ItemCell Item | Empty | Wall

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
handleEvent g (VtyEvent (V.EvKey V.KUp [])) = continue $ movePlayer MUp g
handleEvent g (VtyEvent (V.EvKey V.KDown [])) = continue $ movePlayer MDown g
handleEvent g (VtyEvent (V.EvKey V.KLeft [])) = continue $ movePlayer MLeft g
handleEvent g (VtyEvent (V.EvKey V.KRight [])) = continue $ movePlayer MRight g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g (VtyEvent (V.EvKey V.KEsc [])) = halt g
handleEvent g (AppEvent Tick) = continue $ updateGame g
handleEvent g _ = continue g

updateGame :: Game -> Game
updateGame g =
  let newTime = if g ^. gameStarted then g ^. timeElapsed + 1 else g ^. timeElapsed
      newGame = g & timeElapsed .~ newTime
   in if newTime >= 10
        then g & dead .~ True & gamePassed .~ (g ^. score >= 10)
        else newGame

drawUI :: Game -> [Widget Name]
drawUI g =
  [ C.center $
      hBox
        [ padRight (Pad 3) $ drawStatsAndTimer g,
          drawGrid g,
          padLeft (Pad 3) $ infoBox
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

drawGameOver :: Game -> Widget Name
drawGameOver g =
  if g ^. dead
    then
      withAttr gameOverAttr $
        vBox
          [ C.hCenter $ str "GAME OVER",
            C.hCenter $ str $ if g ^. gamePassed then "Pass" else "Fail"
          ]
    else emptyWidget

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
      | otherwise = Empty

drawCell :: Cell -> Widget Name
drawCell Player = withAttr playerAttr cw
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
      (gameOverAttr, fg V.red `V.withStyle` V.bold),
      (wallAttr, V.white `on` V.white),
      (bronzeAttr, V.red `on` V.red),
      (silverAttr, V.cyan `on` V.cyan),
      (goldAttr, V.yellow `on` V.yellow)
    ]

gameOverAttr :: AttrName
gameOverAttr = "gameOver"

playerAttr, emptyAttr, wallAttr, bronzeAttr, silverAttr, goldAttr :: AttrName
playerAttr = "playerAttr"
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
