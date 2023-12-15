{-# LANGUAGE OverloadedStrings #-}

module UI (drawGame, drawMainMenuUI, handleMainMenuInput) where



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
import Data.Foldable (find, toList)
import Game
  ( Direction (MDown, MLeft, MRight, MUp),
    Game (..),
    InventoryItem (..),
    Item (itemCoord, itemType),
    ItemType (Bomb, Bronze, Silver, Gold, WallBreaker, Teleport),
    dead,
    gamePassed,
    initialGoal,
    movePlayer,
    player,
    playerTrail,
    score,
    startGame,
    timeElapsed,
    selectedDifficulty,
    isDifficultySelection,
  )
import qualified Graphics.Vty as V
import Levels (Level (..), initializeLevels, levelId, levelItems, levelScoreRequired, levelWalls)
import Linear.V2 (V2 (..))
import System.Random (randomRIO)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Maps (Coord, Item (..))
import GameState (GameState(..))

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
handleEvent g event =
  if g ^. Game.isDifficultySelection
    then handleDifficultySelectionEvent g event
    else handleNormalEvent g event

handleNormalEvent :: Game.Game -> BrickEvent Name Tick -> EventM Name (Next Game.Game)
handleNormalEvent g (VtyEvent (V.EvKey (V.KChar 'y') [])) =
  if g ^. Game.dead || g ^. Game.gamePassed
    then restartGame
    else continue g

handleNormalEvent g (VtyEvent (V.EvKey (V.KChar 'n') [])) =
  if g ^. Game.dead || g ^. Game.gamePassed
    then halt g
    else continue g

handleNormalEvent g (VtyEvent (V.EvKey V.KUp [])) =
  if not (g ^. Game.dead || g ^. Game.gamePassed)
    then continue $ Game.movePlayer Game.MUp g
    else continue g

handleNormalEvent g (VtyEvent (V.EvKey V.KDown [])) =
  if not (g ^. Game.dead || g ^. Game.gamePassed)
    then continue $ Game.movePlayer Game.MDown g
    else continue g

handleNormalEvent g (VtyEvent (V.EvKey V.KLeft [])) =
  if not (g ^. Game.dead || g ^. Game.gamePassed)
    then continue $ Game.movePlayer Game.MLeft g
    else continue g

handleNormalEvent g (VtyEvent (V.EvKey V.KRight [])) =
  if not (g ^. Game.dead || g ^. Game.gamePassed)
    then continue $ Game.movePlayer Game.MRight g
    else continue g

handleNormalEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) =
  if not (g ^. Game.dead || g ^. Game.gamePassed)
    then halt g
    else continue g

handleNormalEvent g (VtyEvent (V.EvKey V.KEsc [])) =
  if not (g ^. Game.dead || g ^. Game.gamePassed)
    then halt g
    else continue g

handleNormalEvent g (VtyEvent (V.EvKey (V.KChar 't') [])) =
  if not (g ^. Game.dead || g ^. Game.gamePassed) then do
    let validPlaces = getValidTeleportPlaces g
    case validPlaces of
      [] -> continue g
      _ -> do
        randomIndex <- liftIO $ randomRIO (0, length validPlaces - 1)
        let newPosition = validPlaces !! randomIndex
        let hasTeleportItem = hasTeleport (_inventory g)
        if hasTeleportItem
          then do
            let updatedInventory = consumeTeleportItem (_inventory g)
            continue $ teleportPlayer newPosition (g {_inventory = updatedInventory})
          else continue g
  else continue g

handleNormalEvent g (AppEvent Tick) =
  if g ^. Game.dead || g ^. Game.gamePassed
    then continue g
    else continue $ updateGame fixedDeltaTime g
  where
    fixedDeltaTime = 1.0

handleNormalEvent g _ = continue g


getValidTeleportPlaces :: Game.Game -> [V2 Int]
getValidTeleportPlaces g =
  [ V2 x y | x <- [0 .. currentLevelWidth - 1], y <- [0 .. currentLevelHeight - 1], isValidTeleportPlace g (V2 x y)]
  where
    currentLevel = Game._currentLevel g
    currentLevelHeight = levelHeight currentLevel
    currentLevelWidth = levelWidth currentLevel

isValidTeleportPlace :: Game -> V2 Int -> Bool
isValidTeleportPlace g position =
  position `notElem` (g ^. Game.playerTrail) &&
  position `notElem` (g ^. Game.player) &&
  position `notElem` getWalls g &&
  not (isBombAtPosition position g)

getWalls :: Game -> Seq Coord
getWalls g = _walls g

isBombAtPosition :: Coord -> Game -> Bool
isBombAtPosition position g =
  let bombItems = toList $ Seq.filter (\item -> Game.itemType item == Bomb) (_items g)
  in position `elem` [Game.itemCoord item | item <- bombItems]

teleportPlayer :: V2 Int -> Game.Game -> Game.Game
teleportPlayer newPosition g =
  g & Game.player .~ Seq.singleton newPosition

consumeTeleportItem :: [Game.InventoryItem] -> [Game.InventoryItem]
consumeTeleportItem [] = []
consumeTeleportItem (item : rest) =
  if itemName item == Teleport && itemQuantity item > 0
    then item { itemQuantity = itemQuantity item - 1 } : rest
    else item : consumeTeleportItem rest

hasTeleport :: [InventoryItem] -> Bool
hasTeleport inventory =
  case find (\item -> itemQuantity item > 0 && itemName item == Teleport) inventory of
    Just _ -> True
    Nothing -> False

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
drawUI g
  | g ^. Game.isDifficultySelection = [drawDifficultySelection g]
  | otherwise =
      [ C.center $
          if g ^. Game.dead || g ^. Game.gamePassed
          then drawStatus g
          else
            vBox
            [
              padLeft (Pad 15) $ emojibox,
              hBox
                [ padRight (Pad 3) infoBox,
                  padRight (Pad 2) $ drawInventory (Game._inventory g),
                  padRight (Pad 3) $ drawGoal g,
                  drawGrid g
                ]
            ]
      ]

drawInventory :: [Game.InventoryItem] -> Widget n
drawInventory inv =
  withBorderStyle BS.unicodeBold $
    B.borderWithLabel (str "Inventory") $
      vBox
        -- [ padLeftRight 1 $ padAll 1 $ withAttr bronzeAttr $ str $ "Fries: " ++ showQuantity 0,
        --   padLeftRight 1 $ padAll 1 $ withAttr silverAttr $ str $ "Hotdog: " ++ showQuantity 1,
        --   padLeftRight 1 $ padAll 1 $ withAttr goldAttr $ str $ "Burger: " ++ showQuantity 2,
          [padLeftRight 1 $ padAll 1 $ withAttr wallBreakerAttr $ str $ "WallBreaker: " ++ showQuantity 3,
          padLeftRight 1 $ padAll 1 $ withAttr wallBreakerAttr $ str $ "Teleport: " ++ showQuantity 4
          -- padLeftRight 1 $ padAll 1 $ withAttr bombAttr $ str $ "Bomb: " ++ showQuantity 5
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
drawCell Player = withAttr playerAttr (str playerChar)
drawCell PlayerTrail = withAttr playerTrailAttr (str playerTrailChar)
drawCell (ItemCell item) =
  case itemType item of
    Bronze -> withAttr bronzeAttr (str bronzeChar)
    Silver -> withAttr silverAttr (str silverChar)
    Gold -> withAttr goldAttr (str goldChar)
    WallBreaker-> withAttr wallBreakerAttr (str wallBreakerChar)
    Teleport -> withAttr teleportAttr (str teleportChar)
    Bomb -> withAttr bombAttr (str bombChar)
drawCell Empty = withAttr emptyAttr (str eChar)
drawCell Wall = withAttr wallAttr (str wallChar)


eChar :: String
eChar = "🟦 "

wallBreakerChar :: String
wallBreakerChar="🧨 "

playerChar :: String
playerChar = "🚒 "

playerTrailChar :: String
playerTrailChar ="🚩 "

goldChar :: String
goldChar="🍔 "

silverChar :: String
silverChar="🌭 "

bronzeChar :: String
bronzeChar="🍟 "

bombChar :: String
bombChar="🎆 "

wallChar :: String
wallChar="🧱 "

teleportChar :: String
teleportChar="🚀 "


theMap :: AttrMap
theMap =
  attrMap
    V.defAttr
    [ (playerAttr, V.blue `on` V.black),
      (playerTrailAttr, V.white `on` V.black),
      (gameOverAttr, fg V.red `V.withStyle` V.bold),
      (gamePassedAttr, fg V.green `V.withStyle` V.bold),
      (wallAttr, V.white `on` V.black),
      (bronzeAttr, V.magenta `on` V.black),
      (silverAttr, V.cyan `on` V.black),
      (goldAttr, V.yellow `on` V.black),
      (wallBreakerAttr, V.green `on` V.black),
      (teleportAttr, V.green `on` V.black),
      (bombAttr, V.red `on` V.black),
      (levelAttr, fg V.blue),
      (whiteTextAttr, fg V.white),
      (selectedDifficultyAttr, V.white `on` V.blue)
    ]

playerAttr, playerTrailAttr, emptyAttr, wallAttr, bronzeAttr, silverAttr, goldAttr, wallBreakerAttr, teleportAttr, bombAttr, levelAttr, gameOverAttr, gamePassedAttr, whiteTextAttr :: AttrName
playerAttr = "playerAttr"
playerTrailAttr = "playerTrailAttr"
emptyAttr = "emptyAttr"
wallAttr = "wallAttr"
bronzeAttr = "bronzeAttr"
silverAttr = "silverAttr"
goldAttr = "goldAttr"
wallBreakerAttr = "wallBreakerAttr"
teleportAttr = "teleportAttr"
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
              str "  Collect items to reach the goal in limited time",
              str "  Please avoid walls and bombs",
              str "  You cannot walk back",
              str "\n",
              str "Controls:",
              str "  ↑: Move Up",
              str "  ↓: Move Down",
              str "  ←: Move Left",
              str "  →: Move Right",
              str "  q/Esc: to quit in the game",
              str "  t: Use Teleport Item",
              str "\n",
              str "Item Values:",
              str "  Fries : 1  | Hotdog : 2 | Burger : 5 ",
              str "\n",
              str "Consumable:",
              str "  Wall Breaker: You can go through one wall",
              str "  Teleport   : Teleport to random place",
              str "  More consumables is on the way...."
            ]

emojibox :: Widget Name
emojibox =
  vLimit 20 $
    hLimit 100 $
      withBorderStyle BS.unicodeBold $
        vBox
          [ str "Player :🚒  Fries :🍟  Wall :🧱   Trail :🚩  Burger :🍔  Wall Breaker :🧨  Teleport :🚀  bomb :🎆"
          ]



drawDifficultySelection :: Game.Game -> Widget Name
drawDifficultySelection g =
  let levels = ["Level 1", "Level 2", "Level 3", "Level 4"]
      selectedDifficulty = g ^. Game.selectedDifficulty
      levelWidgets = zipWith (drawDifficulty selectedDifficulty) [0..] levels
  in withBorderStyle BS.unicodeBold $
       B.borderWithLabel (str "Select Difficulty") $
         vBox levelWidgets

drawDifficulty :: Int -> Int -> String -> Widget Name
drawDifficulty selectedDifficulty levelIndex levelName =
  let isSelected = selectedDifficulty == levelIndex
      selectAttr = if isSelected then withAttr selectedDifficultyAttr else id
  in selectAttr $ C.hCenter $ str levelName

selectedDifficultyAttr :: AttrName
selectedDifficultyAttr = "selectedDifficultyAttr"

handleDifficultySelectionEvent :: Game.Game -> BrickEvent Name Tick -> EventM Name (Next Game.Game)
handleDifficultySelectionEvent g (VtyEvent ev) =
  case ev of
    V.EvKey V.KUp [] -> 
      let newDifficulty = max 0 (g ^. Game.selectedDifficulty - 1)
      in continue $ g & Game.selectedDifficulty .~ newDifficulty

    V.EvKey V.KDown [] -> 
      let newDifficulty = min 3 (g ^. Game.selectedDifficulty + 1)
      in continue $ g & Game.selectedDifficulty .~ newDifficulty

    V.EvKey V.KEnter [] -> 
      let selectedDifficulty = g ^. Game.selectedDifficulty
      in do
        newGame <- liftIO $ startGameWithDifficulty selectedDifficulty
        continue newGame

    _ -> continue g
handleDifficultySelectionEvent g _ = continue g

startGameWithDifficulty :: Int -> IO Game.Game
startGameWithDifficulty levelIndex = do
  lv <- initializeLevels
  let initialLevel = head lv
  let xm = levelWidth initialLevel `div` 2
  let ym = levelHeight initialLevel `div` 2
  let initialInventory = [InventoryItem Bronze 0, InventoryItem Silver 0, InventoryItem Gold 0, InventoryItem WallBreaker 0, InventoryItem Teleport 0, InventoryItem Bomb 0]
  let g =
        Game
          { 
            _player = Seq.singleton (V2 xm ym),
            _playerTrail = Seq.empty,
            _items = levelItems initialLevel,
            _score = 0,
            _dir = MUp,
            _dead = False,
            _walls = levelWalls initialLevel,
            _gameStarted = False,
            _timeElapsed = levelTimeRequired initialLevel,
            _gamePassed = False,
            _initialGoal = levelScoreRequired initialLevel,
            _initialTime = levelTimeRequired initialLevel,
            _inventory = initialInventory,
            _currentLevel = initialLevel,
            _levels = lv,
            _isDifficultySelection = False,
            _selectedDifficulty = levelIndex
          }
  return g
handleLevelSelectionEvent g _ = continue g



drawMainMenuUI :: IO ()
drawMainMenuUI = do
    putStrLn "Welcome to the Game!\n"
    putStrLn "Join a man on his quest to taste the world's most delicious foods. \nHe travels far and wide, but mischievous foes stand in his way. \nHelp him outsmart these challenges and fulfill his culinary dream. \nReady for a flavorful adventure?\n\n"
    putStrLn "Please select an option:"
    putStrLn "1. Start Game"
    putStrLn "2. Exit"


handleMainMenuInput :: V.Event -> GameState
handleMainMenuInput (V.EvKey (V.KChar '1') []) = InGame
handleMainMenuInput (V.EvKey (V.KChar '2') []) = Exiting
handleMainMenuInput _ = MainMenu
