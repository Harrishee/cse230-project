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
