-- {-# LANGUAGE OverloadedStrings #-}

import Control.Exception
import Data.IORef
import Data.Text (Text, pack)
import Foreign.C.Types (CInt)
import qualified SDL
import System.Exit
import System.IO

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
    { gameWindow :: SDL.Window
    , gameRenderer :: SDL.Renderer
    , gameCleanActs :: IORef [IO ()]
    }

addClean :: IORef [IO ()] -> IO () -> IO ()
addClean ref action = do
    actions <- readIORef ref
    writeIORef ref (action : actions)

errorClean :: IORef [IO ()] -> SomeException -> String -> IO a
errorClean ref e msg = do
    hPutStrLn stderr (msg ++ show e)
    actions <- readIORef ref
    sequence_ actions
    exitFailure

exitClean :: IORef [IO ()] -> IO ()
exitClean ref = do
    actions <- readIORef ref
    sequence_ actions
    exitSuccess

initSDL :: IO GameData
initSDL = do
    cleanActs <- newIORef [putStrLn "All Clean."]

    catch
        SDL.initializeAll
        ( \e -> do
            errorClean cleanActs e "Error initializing SDL2: "
        )
    addClean cleanActs SDL.quit

    window <-
        catch
            (SDL.createWindow windowTitle myWindowConfig)
            ( \e -> do
                errorClean cleanActs e "Error creating the Window: "
            )
    addClean cleanActs $ SDL.destroyWindow window

    renderer <-
        catch
            (SDL.createRenderer window (-1) SDL.defaultRenderer)
            ( \e -> do
                errorClean cleanActs e "Error creation the Renderer: "
            )
    addClean cleanActs $ SDL.destroyRenderer renderer

    return
        GameData
            { gameWindow = window
            , gameRenderer = renderer
            , gameCleanActs = cleanActs
            }

main :: IO ()
main = do
    gameData <- initSDL

    let renderer = gameRenderer gameData
        cleanActs = gameCleanActs gameData

    SDL.clear renderer

    SDL.present renderer

    SDL.delay 5000

    exitClean cleanActs