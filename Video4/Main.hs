import           Control.Exception
import           Control.Monad.State
import           Data.Text           (Text, pack)
import           Foreign.C.Types     (CInt)
import qualified SDL
import qualified SDL.Image
import           System.Exit
import           System.IO
import           System.Random       (randomRIO)

windowTitle :: Text
windowTitle = pack "04 Changing Colors and Icon"

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
    , gameBackground :: SDL.Texture
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

initSDL :: StateT GameState IO (SDL.Window, SDL.Renderer)
initSDL = do
    addClean $ putStrLn "All Clean!"

    safeRun
        SDL.initializeAll
        "Error initializing SDL2"
    addClean SDL.quit

    safeRun
        (SDL.Image.initialize [SDL.Image.InitPNG])
        "Error initializing SDL2 Image"
    addClean SDL.Image.quit

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

    icon <-
        safeRun
            (SDL.Image.load "images/haskell-logo.png")
            "Error loading Surface"
    SDL.setWindowIcon window icon
    SDL.freeSurface icon

    return (window, renderer)

loadMedia :: (SDL.Window, SDL.Renderer) -> StateT GameState IO GameData
loadMedia (window, renderer) = do
    background <-
        safeRun
            (SDL.Image.loadTexture renderer "images/background.png")
            "Error loading Texture"
    addClean $ SDL.destroyTexture background

    return
        GameData
            { gameWindow = window
            , gameRenderer = renderer
            , gameBackground = background
            }

setRendererColor :: SDL.Renderer -> IO ()
setRendererColor renderer = do
    r <- randomRIO (0, 255)
    g <- randomRIO (0, 255)
    b <- randomRIO (0, 255)

    SDL.rendererDrawColor renderer SDL.$= SDL.V4 r g b 255

handleEvents :: GameData -> [SDL.Event] -> StateT GameState IO ()
handleEvents _ [] = return ()
handleEvents gameData (event : events) = do
    let renderer = gameRenderer gameData
    case SDL.eventPayload event of
        SDL.KeyboardEvent keyboardEvent
            | SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed ->
                case SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) of
                    SDL.KeycodeEscape -> exitClean
                    SDL.KeycodeSpace  -> liftIO $ setRendererColor renderer
                    _                 -> return ()
        SDL.QuitEvent -> exitClean
        _ -> return ()
    handleEvents gameData events

gameLoop :: GameData -> StateT GameState IO ()
gameLoop gameData = do
    let renderer = gameRenderer gameData
        background = gameBackground gameData

    SDL.pollEvents >>= handleEvents gameData

    SDL.clear renderer

    SDL.copy renderer background Nothing Nothing

    SDL.present renderer

    SDL.delay 16

    gameLoop gameData

main :: IO ()
main = do
    evalStateT (initSDL >>= loadMedia >>= gameLoop) initialGameState
