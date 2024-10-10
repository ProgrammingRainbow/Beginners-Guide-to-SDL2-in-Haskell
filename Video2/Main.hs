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
    actions <- get
    put $ actions{gameActions = action : gameActions actions}

errorClean :: [IO ()] -> String -> SomeException -> IO a
errorClean actions errorMsg e = do
    liftIO $ hPutStrLn stderr $ errorMsg ++ ":"
    liftIO $ hPrint stderr e
    liftIO $ sequence_ actions
    liftIO exitFailure

exitClean :: StateT GameState IO ()
exitClean = do
    actions <- gets gameActions
    liftIO $ sequence_ actions
    liftIO exitSuccess

safeRun :: IO a -> String -> StateT GameState IO a
safeRun action errorMsg = do
    actions <- gets gameActions
    liftIO $ catch action $ errorClean actions errorMsg

initSDL :: StateT GameState IO GameData
initSDL = do
    addClean $ putStrLn "All Clean."

    safeRun
        SDL.initializeAll
        "Error initializing SDL2"
    addClean SDL.quit

    window <-
        safeRun
            (SDL.createWindow windowTitle myWindowConfig)
            "Error creating the Window"
    addClean $ SDL.destroyWindow window

    renderer <-
        safeRun
            (SDL.createRenderer window (-1) SDL.defaultRenderer)
            "Error creating the Renderer"
    addClean $ SDL.destroyRenderer renderer

    return
        GameData
            { gameWindow = window
            , gameRenderer = renderer
            }

handleEvents :: GameData -> [SDL.Event] -> StateT GameState IO ()
handleEvents _ [] = return ()
handleEvents gameData (event : rest) = do
    case SDL.eventPayload event of
        SDL.KeyboardEvent keyboardEvent
            | SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed ->
                case SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) of
                    SDL.KeycodeEscape -> exitClean
                    _                 -> return ()
        SDL.QuitEvent -> exitClean
        _ -> return ()
    handleEvents gameData rest

gameLoop :: GameData -> StateT GameState IO ()
gameLoop gameData = do
    let renderer = gameRenderer gameData

    SDL.pollEvents >>= handleEvents gameData

    SDL.clear renderer

    SDL.present renderer

    SDL.delay 16

    gameLoop gameData

main :: IO ()
main = do
    evalStateT (initSDL >>= gameLoop) initialGameState
