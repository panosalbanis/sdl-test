module Main where

import BasicPrelude
import Graphics.UI.SDL as SDL

import Foreign
import Data.Typeable
import Data.Char

import Control.Concurrent
import System.Environment
import System.Exit
import System.Random

type MoveVector = (Int, Int)
type Position = (Int, Int)

data GameState = GameState
    { currentPosition :: Position
    , obstacles :: [Rect]
    }

width = 640
height = 480

main = withInit [InitVideo] $
    do screen <- setVideoMode 640 480 16 [SWSurface]
       setCaption "Test" ""
       enableUnicode True
       let initialGameState = GameState (0, 0) [Rect 310 410 150 70]
       display initialGameState
       enableKeyRepeat 1 1
       loop initialGameState

display :: GameState -> IO ()
display s@(GameState { currentPosition = (x, y) }) = do
         screen <- getVideoSurface
         let format = surfaceGetPixelFormat screen
         red <- mapRGB format 0xFF 0 0
         green <- mapRGB format 0 0xFF 0
         fillRect screen Nothing green
         fillRect screen (Just (Rect x y 10 10)) red
         mapM_ (\x -> fillRect screen (Just x) red) $ obstacles s
         SDL.flip screen

loop :: GameState -> IO GameState
loop s@(GameState { currentPosition = (x, y) }) = do
    event <- pollEvent
    newPosition <- case event of
        Quit -> exitWith ExitSuccess
        KeyDown (Keysym _ _ 'q') -> exitWith ExitSuccess
        KeyDown (Keysym _ _ 'h') -> return $ move (x, y) (-10, 0)
        KeyDown (Keysym _ _ 'j') -> return $ move (x, y) (0, 10)
        KeyDown (Keysym _ _ 'k') -> return $ move (x, y) (0, -10)
        KeyDown (Keysym _ _ 'l') -> return $ move (x, y) (10, 0)
        _ -> return (x, y)
    newPosition <- return $ gravity newPosition
    let newGameState = s { currentPosition = newPosition }
    display newGameState
    threadDelay $ floor $ 1.0 / 5.0
    loop newGameState

gravity :: Position -> Position
gravity s = move s (0, 1)

isWithinScreen :: Position -> Bool
isWithinScreen (x, y) = x < 640 && x >= 0 && y < 480 && y >= 0

move :: Position -> MoveVector -> Position
move (x, y) (x', y') = let newPosition = (x + x', y + y')
                        in if isWithinScreen newPosition then newPosition else (x, y)
