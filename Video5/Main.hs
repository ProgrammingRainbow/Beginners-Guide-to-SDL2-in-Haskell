import           Control.Exception
import           Data.IORef
import           Data.Text         (Text, pack)
import           Foreign.C.Types   (CInt)
import qualified SDL
import qualified SDL.Font
import qualified SDL.Image
import           System.Exit
import           System.IO
import           System.Random     (randomRIO)

windowTitle :: Text
windowTitle = pack "05 Creating Text"

screenWidth, screenHeight :: CInt
screenWidth = 800
screenHeight = 600

fontText :: Text
fontText = pack "Haskell"

fontColor :: SDL.Font.Color
fontColor = SDL.V4 255 255 255 255

fontSize :: Int
fontSize = 60

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
    , gameText       :: SDL.Texture
    , gameTextRect   :: SDL.Rectangle CInt
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

    safeRun
        SDL.Font.initialize
        "Error initializing SDL2 Font"
        actionsRef
    addClean actionsRef SDL.Font.quit

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

rectFromTexture :: SDL.Texture -> IO (SDL.Rectangle CInt)
rectFromTexture texture = do
    SDL.TextureInfo
        { SDL.textureWidth = textureWidth
        , SDL.textureHeight = textureHeight
        } <-
        SDL.queryTexture texture
    return $ SDL.Rectangle (SDL.P (SDL.V2 0 0)) (SDL.V2 textureWidth textureHeight)

loadMedia :: (SDL.Window, SDL.Renderer, IORef [IO ()]) -> IO GameData
loadMedia (window, renderer, actionsRef) = do
    background <-
        safeRun
            (SDL.Image.loadTexture renderer "images/background.png")
            "Error Loading a Texture"
            actionsRef
    addClean actionsRef $ SDL.destroyTexture background

    font <-
        safeRun
            (SDL.Font.load "fonts/freesansbold.ttf" fontSize)
            "Error creating a Font"
            actionsRef
    addClean actionsRef $ SDL.Font.free font

    fontSurf <-
        safeRun
            (SDL.Font.blended font fontColor fontText)
            "Error creating a Surface from Font"
            actionsRef
    addClean actionsRef $ SDL.freeSurface fontSurf

    text <-
        safeRun
            (SDL.createTextureFromSurface renderer fontSurf)
            "Error creating a Texture from Surface"
            actionsRef
    addClean actionsRef $ SDL.destroyTexture text

    textRect <-
        safeRun
            (rectFromTexture text)
            "Error querying Texture"
            actionsRef

    return
        GameData
            { gameWindow = window
            , gameRenderer = renderer
            , gameBackground = background
            , gameText = text
            , gameTextRect = textRect
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
                    SDL.KeycodeSpace  -> setRendererColor renderer
                    _                 -> return ()
        SDL.QuitEvent -> exitClean actionsRef
        _ -> return ()
    handleEvents gameData rest

gameLoop :: GameData -> IO ()
gameLoop gameData = do
    let renderer = gameRenderer gameData
        background = gameBackground gameData
        text = gameText gameData
        textRect = gameTextRect gameData

    SDL.pollEvents >>= handleEvents gameData

    SDL.clear renderer

    SDL.copy renderer background Nothing Nothing
    SDL.copy renderer text Nothing $ Just textRect

    SDL.present renderer

    SDL.delay 16

    gameLoop gameData

main :: IO ()
main = do
    gameData <- initSDL >>= loadMedia

    gameLoop gameData
