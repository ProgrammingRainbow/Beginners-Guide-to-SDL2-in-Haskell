import           Control.Exception
import           Control.Monad.State
import           Data.Text           (Text, pack)
import           Foreign.C.Types     (CInt)
import qualified SDL
import qualified SDL.Font
import qualified SDL.Image
import           System.Exit
import           System.IO
import           System.Random       (randomRIO)

windowTitle :: Text
windowTitle = pack "06 Moving Text"

screenWidth, screenHeight :: CInt
screenWidth = 800
screenHeight = 600

fontText :: Text
fontText = pack "Haskell"

fontColor :: SDL.Font.Color
fontColor = SDL.V4 255 255 255 255

fontSize :: Int
fontSize = 60

textVel :: CInt
textVel = 3

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
    }

data GameState = GameState
    { gameActions  :: [IO ()]
    , gameTextRect :: SDL.Rectangle CInt
    , gameTextVel  :: (CInt, CInt)
    }

initialGameState :: GameState
initialGameState =
    GameState
        { gameActions = []
        , gameTextRect = SDL.Rectangle (SDL.P (SDL.V2 0 0)) (SDL.V2 0 0)
        , gameTextVel = (textVel, textVel)
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

    safeRun
        SDL.Font.initialize
        "Error initializing SDL2 Font"
    addClean SDL.Font.quit

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

rectFromSurface :: SDL.Surface -> IO (SDL.Rectangle CInt)
rectFromSurface surface = do
    SDL.V2 surfaceW surfaceH <- SDL.surfaceDimensions surface
    return $ SDL.Rectangle (SDL.P (SDL.V2 0 0)) (SDL.V2 surfaceW surfaceH)

loadMedia :: (SDL.Window, SDL.Renderer) -> StateT GameState IO GameData
loadMedia (window, renderer) = do
    background <-
        safeRun
            (SDL.Image.loadTexture renderer "images/background.png")
            "Error loading Texture"
    addClean $ SDL.destroyTexture background

    font <-
        safeRun
            (SDL.Font.load "fonts/freesansbold.ttf" fontSize)
            "Error creating Font"
    addClean $ SDL.Font.free font

    fontSurf <-
        safeRun
            (SDL.Font.blended font fontColor fontText)
            "Error creating Surface from Font"
    addClean $ SDL.freeSurface fontSurf

    text <-
        safeRun
            (SDL.createTextureFromSurface renderer fontSurf)
            "Error creating Texture from Surface"
    addClean $ SDL.destroyTexture text

    sprite <-
        safeRun
            (SDL.Image.loadTexture renderer "images/sdl-logo.png")
            "Error loading a Texture"
    addClean $ SDL.destroyTexture sprite

    textRect <-
        safeRun
            (rectFromSurface fontSurf)
            "Error creating Rectange from Surface"

    modify $ \gameState -> gameState{gameTextRect = textRect}

    return
        GameData
            { gameWindow = window
            , gameRenderer = renderer
            , gameBackground = background
            , gameText = text
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

textUpdate :: StateT GameState IO ()
textUpdate = do
    gameState <- get

    let SDL.Rectangle (SDL.P (SDL.V2 oldX oldY)) (SDL.V2 w h) = gameTextRect gameState
    let (xVel, yVel) = gameTextVel gameState

    let (newX, newXVel)
            | xVel < 0 && oldX < 0 = (0, xVel * (-1))
            | xVel > 0 && (oldX + w) > screenWidth = (screenWidth - w, xVel * (-1))
            | otherwise = (oldX + xVel, xVel)

    let (newY, newYVel)
            | yVel < 0 && oldY < 0 = (0, yVel * (-1))
            | yVel > 0 && (oldY + h) > screenHeight = (screenHeight - h, yVel * (-1))
            | otherwise = (oldY + yVel, yVel)

    let textRect = SDL.Rectangle (SDL.P (SDL.V2 newX newY)) (SDL.V2 w h)

    put
        gameState
            { gameTextRect = textRect
            , gameTextVel = (newXVel, newYVel)
            }

gameLoop :: GameData -> StateT GameState IO ()
gameLoop gameData = do
    let renderer = gameRenderer gameData
        background = gameBackground gameData
        text = gameText gameData

    SDL.pollEvents >>= handleEvents gameData

    textUpdate
    textRect <- gets gameTextRect

    SDL.clear renderer

    SDL.copy renderer background Nothing Nothing
    SDL.copy renderer text Nothing $ Just textRect

    SDL.present renderer

    SDL.delay 16

    gameLoop gameData

main :: IO ()
main = do
    evalStateT (initSDL >>= loadMedia >>= gameLoop) initialGameState
