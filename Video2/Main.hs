import           Control.Exception
import           Control.Monad.State
import           Data.Text           (Text, pack)
import           Foreign.C.Types     (CInt)
import qualified SDL
import           System.Exit
import           System.IO

windowTitle :: Text
windowTitle = pack "02 Close Window"

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
    { gameWindow   :: SDL.Window
    , gameRenderer :: SDL.Renderer
    }

newtype GameState = GameState
    { gameActions :: [IO ()]
    }

initialGameState :: GameState
initialGameState =
    GameState
        { gameActions = []
        }

addClean :: IO () -> StateT GameState IO ()
addClean action = do
    modify (\gameState -> gameState{gameActions = action : gameActions gameState})

exitClean :: StateT GameState IO ()
exitClean = do
    actions <- gets gameActions
    liftIO $ sequence_ actions
    liftIO exitSuccess

errorClean :: [IO ()] -> String -> SomeException -> IO a
errorClean actions errorMsg e = do
    liftIO $ hPutStrLn stderr $ errorMsg ++ ":"
    liftIO $ hPrint stderr e
    liftIO $ sequence_ actions
    liftIO exitFailure

safeRun :: IO a -> String -> StateT GameState IO a
safeRun action errorMsg = do
    actions <- gets gameActions
    liftIO $ catch action $ errorClean actions errorMsg

initSDL :: StateT GameState IO GameData
initSDL = do
    addClean $ putStrLn "All Clean!"

    safeRun
        SDL.initializeAll
        "Error initializing SDL2"
    addClean SDL.quit

    window <-
        safeRun
            (SDL.createWindow windowTitle myWindowConfig)
            "Error creating Window"
    addClean $ SDL.destroyWindow window

    renderer <-
        safeRun
            (SDL.createRenderer window (-1) SDL.defaultRenderer)
            "Error creating Renderer"
    addClean $ SDL.destroyRenderer renderer

    return
        GameData
            { gameWindow = window
            , gameRenderer = renderer
            }

handleEvents :: [SDL.Event] -> StateT GameState IO ()
handleEvents [] = return ()
handleEvents (event : events) = do
    case SDL.eventPayload event of
        SDL.KeyboardEvent keyboardEvent
            | SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed ->
                case SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) of
                    SDL.KeycodeEscape -> exitClean
                    _                 -> return ()
        SDL.QuitEvent -> exitClean
        _ -> return ()
    handleEvents events

gameLoop :: GameData -> StateT GameState IO ()
gameLoop gameData = do
    let renderer = gameRenderer gameData

    SDL.pollEvents >>= handleEvents

    SDL.clear renderer

    SDL.present renderer

    SDL.delay 16

    gameLoop gameData

main :: IO ()
main = do
    evalStateT (initSDL >>= gameLoop) initialGameState
