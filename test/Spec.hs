import Game
import Levels (Level(..))
import Maps (Coord, Item(..))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Linear.V2 (V2(..))

-- Test movePlayer function
testMovePlayer :: Bool
testMovePlayer = and
  [ testPlayerMoves,
    testPlayerHitsWall,
    testPlayerHitsBomb,
    testPlayerUsesWallBreaker
  ]

-- Test player moves successfully without obstacles
testPlayerMoves :: Bool
testPlayerMoves =
  let game = createMockGame (V2 1 1) [] [] False MRight False
      newGame = movePlayer MRight game
      expectedPosition = V2 2 1
  in getPlayerPosition newGame == expectedPosition

-- Test player hits a wall and does not move
testPlayerHitsWall :: Bool
testPlayerHitsWall =
  let game = createMockGame (V2 1 1) [V2 2 1] [] False MRight False
      newGame = movePlayer MRight game
  in getPlayerPosition newGame == V2 1 1

-- Test player hits a bomb and dies
testPlayerHitsBomb :: Bool
testPlayerHitsBomb =
  let game = createMockGame (V2 1 1) [] [V2 2 1] False MRight False
      newGame = movePlayer MRight game
  in gameIsDead newGame

-- Test player uses a wall breaker to pass through a wall
testPlayerUsesWallBreaker :: Bool
testPlayerUsesWallBreaker =
  let game = createMockGame (V2 1 1) [V2 2 1] [] True MRight False
      newGame = movePlayer MRight game
      expectedPosition = V2 2 1
  in getPlayerPosition newGame == expectedPosition

-- Create a mock Game state
createMockGame :: V2 Int -> [Coord] -> [Coord] -> Bool -> Direction -> Bool -> Game
createMockGame playerPos walls bombs hasWallBreaker dir gameStarted =
  let items = map (\coord -> Item Bomb coord) bombs
      inventory = if hasWallBreaker then [InventoryItem WallBreaker 1] else []
      mockLevel = Level {
                      levelId = 1,
                      levelItems = Seq.fromList [], -- Empty sequence for mock
                      levelWalls = Seq.fromList [], -- Empty sequence for mock
                      levelScoreRequired = 0,
                      levelHeight = 10, -- Arbitrary value
                      levelWidth = 10, -- Arbitrary value
                      levelTimeRequired = 10 -- Arbitrary value
                    }
  in Game { _player = Seq.singleton playerPos,
            _walls = Seq.fromList walls,
            _items = Seq.fromList items,
            _inventory = inventory,
            _dir = dir,
            _gameStarted = gameStarted,
            _playerTrail = Seq.empty,
            _score = 0,
            _dead = False,
            _timeElapsed = 0,
            _gamePassed = False,
            _initialGoal = 10, -- Arbitrary value
            _initialTime = 10, -- Arbitrary value
            _currentLevel = mockLevel,
            _levels = [mockLevel], -- List containing just the mock level
            _isDifficultySelection = False,
            _selectedDifficulty = 0
          }


getPlayerPosition :: Game -> V2 Int
getPlayerPosition game = case _player game of
  (p Seq.:<| _) -> p
  _ -> error "Player position not found"

gameIsDead :: Game -> Bool
gameIsDead = _dead

-- Main function to run all tests
main :: IO ()
main = do
  putStrLn "Running unit tests for movePlayer:"
  putStrLn $ "Test player moves: " ++ show testPlayerMoves
  putStrLn $ "Test player hits wall: " ++ show testPlayerHitsWall
  putStrLn $ "Test player hits bomb: " ++ show testPlayerHitsBomb
  putStrLn $ "Test player uses wall breaker: " ++ show testPlayerUsesWallBreaker
