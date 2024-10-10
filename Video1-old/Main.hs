-- {-# LANGUAGE OverloadedStrings #-}

import           Control.Exception
import           Data.IORef
import           Data.Text         (Text, pack)
import           Foreign.C.Types   (CInt)
import qualified SDL
import           System.Exit
import           System.IO

windowTitle :: Text
windowTitle = pack "01 Open Window"

screenWidth, screenHeight :: CInt
screenWidth = 800
screenHeight = 600

myWindowConfig :: SDL.WindowConfig
myWindowConfig =
    SDL.defaultWindow
        { SDL.windowPosition = SDL.Centered
        , SDL.windowInitialSize = SDL.V2 screenWidth screenHeight
        }

data GameData = GameData
    { gameWindow     :: SDL.Window
    , gameRenderer   :: SDL.Renderer
    , gameActionsRef :: IORef [IO ()]
    }

addClean :: IORef [IO ()] -> IO () -> IO ()
addClean actionsRef action = do
    actions <- readIORef actionsRef
    writeIORef actionsRef (action : actions)

errorClean :: IORef [IO ()] -> String -> SomeException -> IO a
errorClean actionsRef errorMsg e = do
    hPutStrLn stderr $ errorMsg ++ ":"
    hPrint stderr e
    actions <- readIORef actionsRef
    sequence_ actions
    exitFailure

exitClean :: IORef [IO ()] -> IO ()
exitClean actionsRef = do
    actions <- readIORef actionsRef
    sequence_ actions
    exitSuccess

safeRun :: IO a -> String -> IORef [IO ()] -> IO a
safeRun action errorMsg actionsRef =
    catch action $ \e -> errorClean actionsRef errorMsg e

initSDL :: IO GameData
initSDL = do
    actionsRef <- newIORef [putStrLn "All Clean."]

    safeRun
        SDL.initializeAll
        "Error initialize SDL2"
        actionsRef
    addClean actionsRef SDL.quit

    window <-
        safeRun
            (SDL.createWindow windowTitle myWindowConfig)
            "Error creating the Window"
            actionsRef
    addClean actionsRef $ SDL.destroyWindow window

    renderer <-
        safeRun
            (SDL.createRenderer window (-1) SDL.defaultRenderer)
            "Error creation the Renderer"
            actionsRef
    addClean actionsRef $ SDL.destroyRenderer renderer

    return
        GameData
            { gameWindow = window
            , gameRenderer = renderer
            , gameActionsRef = actionsRef
            }

main :: IO ()
main = do
    gameData <- initSDL

    let renderer = gameRenderer gameData
        actionsRef = gameActionsRef gameData

    SDL.clear renderer

    SDL.present renderer

    SDL.delay 5000

    exitClean actionsRef
