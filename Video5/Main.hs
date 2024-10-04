import Control.Exception
import Data.IORef
import Data.Text (Text, pack)
import Foreign.C.Types (CInt)
import qualified SDL
import qualified SDL.Font
import qualified SDL.Image
import System.Exit
import System.IO
import System.Random (randomRIO)

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
    { gameWindow :: SDL.Window
    , gameRenderer :: SDL.Renderer
    , gameBackground :: SDL.Texture
    , gameText :: SDL.Texture
    , gameTextRect :: SDL.Rectangle CInt
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

initSDL :: IO (SDL.Window, SDL.Renderer, IORef [IO ()])
initSDL = do
    cleanActs <- newIORef [putStrLn "All Clean."]

    catch
        SDL.initializeAll
        ( \e -> do
            errorClean cleanActs e "Error initializing SDL2: "
        )
    addClean cleanActs SDL.quit

    catch
        (SDL.Image.initialize [SDL.Image.InitPNG])
        ( \e -> do
            errorClean cleanActs e "Error initializing SDL2 Image: "
        )
    addClean cleanActs SDL.Image.quit

    catch
        SDL.Font.initialize
        ( \e -> do
            errorClean cleanActs e "Error initializing SDL2 Font: "
        )
    addClean cleanActs SDL.Font.quit

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

    icon <-
        catch
            (SDL.Image.load "images/Haskell-logo.png")
            ( \e -> do
                errorClean cleanActs e "Error loading Surface: "
            )
    SDL.setWindowIcon window icon
    SDL.freeSurface icon

    return (window, renderer, cleanActs)

loadMedia :: (SDL.Window, SDL.Renderer, IORef [IO ()]) -> IO GameData
loadMedia (window, renderer, cleanActs) = do
    background <-
        catch
            (SDL.Image.loadTexture renderer "images/background.png")
            ( \e -> do
                errorClean cleanActs e "Error loading a Texture: "
            )
    addClean cleanActs $ SDL.destroyTexture background

    font <-
        catch
            (SDL.Font.load "fonts/freesansbold.ttf" fontSize)
            ( \e -> do
                errorClean cleanActs e "Error creating a Font: "
            )
    addClean cleanActs $ SDL.Font.free font

    fontSurf <-
        catch
            (SDL.Font.blended font fontColor fontText)
            ( \e -> do
                errorClean cleanActs e "Error creating a Surface from Font: "
            )
    addClean cleanActs $ SDL.freeSurface fontSurf

    text <-
        catch
            (SDL.createTextureFromSurface renderer fontSurf)
            ( \e -> do
                errorClean cleanActs e "Error creating a Texture from Surface: "
            )
    addClean cleanActs $ SDL.destroyTexture text

    textRect <-
        catch
            (rectFromTexture text)
            ( \e -> do
                errorClean cleanActs e "Error querying Texture: "
            )

    return
        GameData
            { gameWindow = window
            , gameRenderer = renderer
            , gameBackground = background
            , gameText = text
            , gameTextRect = textRect
            , gameCleanActs = cleanActs
            }

rectFromTexture :: SDL.Texture -> IO (SDL.Rectangle CInt)
rectFromTexture texture = do
    SDL.TextureInfo
        { SDL.textureWidth = textureWidth
        , SDL.textureHeight = textureHeight
        } <-
        SDL.queryTexture texture
    return $ SDL.Rectangle (SDL.P (SDL.V2 0 0)) (SDL.V2 textureWidth textureHeight)

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
    let cleanActs = gameCleanActs gameData
        renderer = gameRenderer gameData
    case SDL.eventPayload event of
        SDL.KeyboardEvent keyboardEvent
            | SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed ->
                case SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) of
                    SDL.KeycodeEscape -> exitClean cleanActs
                    SDL.KeycodeSpace -> setRendererColor renderer
                    _ -> return ()
        SDL.QuitEvent -> exitClean cleanActs
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
