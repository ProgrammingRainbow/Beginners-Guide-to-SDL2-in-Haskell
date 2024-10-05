import Control.Exception
import Data.IORef
import Data.Text (Text, pack)
import Foreign.C.Types (CInt)
import qualified SDL
import qualified SDL.Image
import System.Exit
import System.IO
import System.Random (randomRIO)

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
    { gameWindow :: SDL.Window
    , gameRenderer :: SDL.Renderer
    , gameBackground :: SDL.Texture
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

initSDL :: IO (SDL.Window, SDL.Renderer, IORef [IO ()])
initSDL = do
    actionsRef <- newIORef [putStrLn "All Clean."]

    safeRun
        SDL.initializeAll
        "Error initialize SDL2"
        actionsRef
    addClean actionsRef SDL.quit

    safeRun
        (SDL.Image.initialize [SDL.Image.InitPNG])
        "Error initializing SDL2 Image"
        actionsRef
    addClean actionsRef SDL.Image.quit

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

    icon <-
        safeRun
            (SDL.Image.load "images/haskell-logo.png")
            "Error loading Surface"
            actionsRef
    SDL.setWindowIcon window icon
    SDL.freeSurface icon

    return (window, renderer, actionsRef)

loadMedia :: (SDL.Window, SDL.Renderer, IORef [IO ()]) -> IO GameData
loadMedia (window, renderer, actionsRef) = do
    background <-
        safeRun
            (SDL.Image.loadTexture renderer "images/background.png")
            "Error Loading a Texture"
            actionsRef
    addClean actionsRef $ SDL.destroyTexture background

    return
        GameData
            { gameWindow = window
            , gameRenderer = renderer
            , gameBackground = background
            , gameActionsRef = actionsRef
            }

setRendererColor :: SDL.Renderer -> IO ()
setRendererColor renderer = do
    r <- randomRIO (0, 255)
    g <- randomRIO (0, 255)
    b <- randomRIO (0, 255)

    let color = SDL.V4 r g b 255
    SDL.rendererDrawColor renderer SDL.$= color

handleEvents :: GameData -> [SDL.Event] -> IO ()
handleEvents _ [] = return ()
handleEvents gameData (event : rest) = do
    let actionsRef = gameActionsRef gameData
        renderer = gameRenderer gameData
    case SDL.eventPayload event of
        SDL.KeyboardEvent keyboardEvent
            | SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed ->
                case SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) of
                    SDL.KeycodeEscape -> exitClean actionsRef
                    SDL.KeycodeSpace -> setRendererColor renderer
                    _ -> return ()
        SDL.QuitEvent -> exitClean actionsRef
        _ -> return ()
    handleEvents gameData rest

gameLoop :: GameData -> IO ()
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
    gameData <- initSDL >>= loadMedia

    gameLoop gameData
