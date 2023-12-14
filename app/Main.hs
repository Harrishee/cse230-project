module Main (main) where

import UI (drawGame, drawMainMenu, handleMainMenuInput)
import GameState (GameState(..))
import qualified Graphics.Vty as V

main :: IO ()
main = gameLoop MainMenu

gameLoop :: GameState -> IO ()
gameLoop state = do
    case state of
        MainMenu -> do
            vty <- V.mkVty V.defaultConfig
            e <- V.nextEvent vty
            let nextState = handleMainMenuInput e
            V.shutdown vty
            gameLoop nextState  
        InGame -> do
            drawGame
        Exiting -> do
            return ()