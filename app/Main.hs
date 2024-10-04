import Control.Exception
import Control.Monad.State
import Data.IORef
import Data.Text (Text, pack)
import Foreign.C.Types (CInt)
import qualified SDL
import qualified SDL.Font
import qualified SDL.Image
import qualified SDL.Mixer
import System.Exit
import System.IO
import System.Random (randomRIO)

windowTitle :: Text
windowTitle = pack "08 Sound Effects and Music"

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

spriteVel :: CInt
spriteVel = 5

myWindowConfig :: SDL.WindowConfig
myWindowConfig =
    SDL.defaultWindow
        { SDL.windowPosition = SDL.Centered
        , SDL.windowInitialSize = SDL.V2 screenWidth screenHeight
        }

myAudio :: SDL.Mixer.Audio
myAudio =
    SDL.Mixer.defaultAudio
        { SDL.Mixer.audioFrequency = 44100
        }

data GameData = GameData
    { gameWindow :: SDL.Window
    , gameRenderer :: SDL.Renderer
    , gameBackground :: SDL.Texture
    , gameText :: SDL.Texture
    , gameSprite :: SDL.Texture
    , gameHaskellSound :: SDL.Mixer.Chunk
    , gameSDLSound :: SDL.Mixer.Chunk
    , gameMusic :: SDL.Mixer.Music
    , gameCleanActs :: IORef [IO ()]
    }

data GameState = GameState
    { gameTextRect :: SDL.Rectangle CInt
    , gameSpriteRect :: SDL.Rectangle CInt
    , gameTextVel :: (CInt, CInt)
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

    catch
        (SDL.Mixer.initialize [SDL.Mixer.InitOGG])
        ( \e -> do
            errorClean cleanActs e "Error initializing SDL2 Mixer: "
        )
    addClean cleanActs SDL.Mixer.quit

    catch
        (SDL.Mixer.openAudio myAudio 1024)
        ( \e -> do
            errorClean cleanActs e "Error initializing SDL2 Mixer: "
        )
    addClean cleanActs SDL.Mixer.closeAudio

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

    sprite <-
        catch
            (SDL.Image.loadTexture renderer "images/SDL.png")
            ( \e -> do
                errorClean cleanActs e "Error loading a Texture: "
            )
    addClean cleanActs $ SDL.destroyTexture sprite

    haskellSound <-
        catch
            (SDL.Mixer.load "sounds/Haskell.ogg")
            ( \e -> do
                errorClean cleanActs e "Error loading Chunk: "
            )
    addClean cleanActs $ SDL.Mixer.free haskellSound

    sdlSound <-
        catch
            (SDL.Mixer.load "sounds/SDL.ogg")
            ( \e -> do
                errorClean cleanActs e "Error loading Chunk: "
            )
    addClean cleanActs $ SDL.Mixer.free sdlSound

    music <-
        catch
            (SDL.Mixer.load "music/freesoftwaresong-8bit.wav")
            ( \e -> do
                errorClean cleanActs e "Error loading Music: "
            )
    addClean cleanActs $ SDL.Mixer.free music

    addClean cleanActs $ SDL.Mixer.halt SDL.Mixer.AllChannels
    addClean cleanActs SDL.Mixer.haltMusic

    return
        GameData
            { gameWindow = window
            , gameRenderer = renderer
            , gameBackground = background
            , gameText = text
            , gameSprite = sprite
            , gameHaskellSound = haskellSound
            , gameSDLSound = sdlSound
            , gameMusic = music
            , gameCleanActs = cleanActs
            }

createState :: GameData -> IO GameState
createState gameData = do
    let cleanActs = gameCleanActs gameData

    textRect <-
        catch
            (rectFromTexture $ gameText gameData)
            ( \e -> do
                errorClean cleanActs e "Error querying Texture: "
            )

    spriteRect <-
        catch
            (rectFromTexture $ gameSprite gameData)
            ( \e -> do
                errorClean cleanActs e "Error querying Texture: "
            )

    return
        GameState
            { gameTextRect = textRect
            , gameSpriteRect = spriteRect
            , gameTextVel = (textVel, textVel)
            }

rectFromTexture :: SDL.Texture -> IO (SDL.Rectangle CInt)
rectFromTexture texture = do
    SDL.TextureInfo
        { SDL.textureWidth = textureWidth
        , SDL.textureHeight = textureHeight
        } <-
        SDL.queryTexture texture
    return $ SDL.Rectangle (SDL.P (SDL.V2 0 0)) (SDL.V2 textureWidth textureHeight)

setRendererColor :: SDL.Renderer -> SDL.Mixer.Chunk -> IO ()
setRendererColor renderer sdlSound = do
    r <- randomRIO (0, 255)
    g <- randomRIO (0, 255)
    b <- randomRIO (0, 255)

    let color = SDL.V4 r g b 255
    SDL.rendererDrawColor renderer SDL.$= color
    playChunk sdlSound

playChunk :: SDL.Mixer.Chunk -> IO ()
playChunk chunk = do
    maybeChannel <- SDL.Mixer.getAvailable SDL.Mixer.DefaultGroup
    case maybeChannel of
        Just channel -> do
            _ <- SDL.Mixer.playOn channel 1 chunk
            return ()
        Nothing -> return ()

handleEvents :: GameData -> [SDL.Event] -> IO ()
handleEvents _ [] = return ()
handleEvents gameData (event : rest) = do
    let cleanActs = gameCleanActs gameData
        renderer = gameRenderer gameData
        sdlSound = gameSDLSound gameData
    case SDL.eventPayload event of
        SDL.KeyboardEvent keyboardEvent
            | SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed ->
                case SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) of
                    SDL.KeycodeEscape -> exitClean cleanActs
                    SDL.KeycodeSpace -> setRendererColor renderer sdlSound
                    _ -> return ()
        SDL.QuitEvent -> exitClean cleanActs
        _ -> return ()
    handleEvents gameData rest

textUpdate :: SDL.Mixer.Chunk -> StateT GameState IO ()
textUpdate haskellSound = do
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

    when (xVel /= newXVel || yVel /= newYVel) $
        liftIO $
            playChunk haskellSound

    put
        gameState
            { gameTextRect = textRect
            , gameTextVel = (newXVel, newYVel)
            }

spriteUpdate :: StateT GameState IO ()
spriteUpdate = do
    gameState <- get
    keyboardState <- SDL.getKeyboardState

    let left = keyboardState SDL.ScancodeLeft || keyboardState SDL.ScancodeA
    let right = keyboardState SDL.ScancodeRight || keyboardState SDL.ScancodeD
    let up = keyboardState SDL.ScancodeUp || keyboardState SDL.ScancodeW
    let down = keyboardState SDL.ScancodeDown || keyboardState SDL.ScancodeS

    let SDL.Rectangle (SDL.P (SDL.V2 oldX oldY)) spriteDim = gameSpriteRect gameState

    let newX
            | left && not right = oldX - spriteVel
            | right && not left = oldX + spriteVel
            | otherwise = oldX

    let newY
            | up && not down = oldY - spriteVel
            | down && not up = oldY + spriteVel
            | otherwise = oldY

    let spriteRect = SDL.Rectangle (SDL.P (SDL.V2 newX newY)) spriteDim
    put gameState{gameSpriteRect = spriteRect}

gameLoop :: GameData -> StateT GameState IO ()
gameLoop gameData = do
    gameState <- get
    let textRect = gameTextRect gameState
        spriteRect = gameSpriteRect gameState

    let renderer = gameRenderer gameData
        background = gameBackground gameData
        text = gameText gameData
        sprite = gameSprite gameData
        haskellSound = gameHaskellSound gameData

    liftIO $ SDL.pollEvents >>= handleEvents gameData

    textUpdate haskellSound
    spriteUpdate

    SDL.clear renderer

    SDL.copy renderer background Nothing Nothing
    SDL.copy renderer text Nothing $ Just textRect
    SDL.copy renderer sprite Nothing $ Just spriteRect

    SDL.present renderer

    SDL.delay 16

    gameLoop gameData

main :: IO ()
main = do
    gameData <- initSDL >>= loadMedia
    gameState <- createState gameData

    SDL.Mixer.playMusic SDL.Mixer.Forever $ gameMusic gameData

    evalStateT (gameLoop gameData) gameState
