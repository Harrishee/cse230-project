module Main (main) where

import UI (drawGame, drawMainMenuUI, handleMainMenuInput)
import GameState (GameState(..))
import qualified Graphics.Vty as V

main :: IO ()
main = gameLoop MainMenu

gameLoop :: GameState -> IO ()
gameLoop state = do
    case state of
        MainMenu -> do
            vty <- V.mkVty V.defaultConfig
            drawMainMenuUI
            e <- V.nextEvent vty
            let nextState = handleMainMenuInput e
            V.shutdown vty
            case nextState of
                InGame -> drawGame
                Exiting -> return ()
        InGame -> drawGame
        Exiting -> return ()