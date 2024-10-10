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

initSDL :: StateT GameState IO (SDL.Window, SDL.Renderer)
initSDL = do
    addClean $ putStrLn "All Clean."

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
            "Error creating the Window"
    addClean $ SDL.destroyWindow window

    renderer <-
        safeRun
            (SDL.createRenderer window (-1) SDL.defaultRenderer)
            "Error creating the Renderer"
    addClean $ SDL.destroyRenderer renderer

    icon <-
        safeRun
            (SDL.Image.load "images/haskell-logo.png")
            "Error loading Surface"
    SDL.setWindowIcon window icon
    SDL.freeSurface icon

    return (window, renderer)

rectFromTexture :: SDL.Texture -> IO (SDL.Rectangle CInt)
rectFromTexture texture = do
    SDL.TextureInfo
        { SDL.textureWidth = textureWidth
        , SDL.textureHeight = textureHeight
        } <-
        SDL.queryTexture texture
    return $ SDL.Rectangle (SDL.P (SDL.V2 0 0)) (SDL.V2 textureWidth textureHeight)

loadMedia :: (SDL.Window, SDL.Renderer) -> StateT GameState IO GameData
loadMedia (window, renderer) = do
    background <-
        safeRun
            (SDL.Image.loadTexture renderer "images/background.png")
            "Error Loading a Texture"
    addClean $ SDL.destroyTexture background

    font <-
        safeRun
            (SDL.Font.load "fonts/freesansbold.ttf" fontSize)
            "Error creating a Font"
    addClean $ SDL.Font.free font

    fontSurf <-
        safeRun
            (SDL.Font.blended font fontColor fontText)
            "Error creating a Surface from Font"
    addClean $ SDL.freeSurface fontSurf

    text <-
        safeRun
            (SDL.createTextureFromSurface renderer fontSurf)
            "Error creating a Texture from Surface"
    addClean $ SDL.destroyTexture text

    textRect <-
        safeRun
            (rectFromTexture text)
            "Error querying Texture"

    return
        GameData
            { gameWindow = window
            , gameRenderer = renderer
            , gameBackground = background
            , gameText = text
            , gameTextRect = textRect
            }

setRendererColor :: SDL.Renderer -> IO ()
setRendererColor renderer = do
    r <- randomRIO (0, 255)
    g <- randomRIO (0, 255)
    b <- randomRIO (0, 255)

    let color = SDL.V4 r g b 255
    SDL.rendererDrawColor renderer SDL.$= color

handleEvents :: GameData -> [SDL.Event] -> StateT GameState IO ()
handleEvents _ [] = return ()
handleEvents gameData (event : rest) = do
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
    handleEvents gameData rest

gameLoop :: GameData -> StateT GameState IO ()
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
    evalStateT (initSDL >>= loadMedia >>= gameLoop) initialGameState
