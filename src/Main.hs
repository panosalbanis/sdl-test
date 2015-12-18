module Main where

import BasicPrelude
import Graphics.UI.SDL as SDL

import Foreign
import Data.Typeable
import Data.Char

import System.Environment
import System.Exit
import System.Random

type GameState = (Int, Int)

width = 640
height = 480

main = withInit [InitVideo] $
    do screen <- setVideoMode 640 480 16 [SWSurface]
       setCaption "Test" ""
       enableUnicode True
       let initialGameState = (0, 0)
       display initialGameState
       loop initialGameState

display :: GameState -> IO ()
display (x, y)
    = do screen <- getVideoSurface
         let format = surfaceGetPixelFormat screen
         red <- mapRGB format 0xFF 0 0
         green <- mapRGB format 0 0xFF 0
         fillRect screen Nothing green
         fillRect screen (Just (Rect x y 10 10)) red
         SDL.flip screen

loop :: GameState -> IO GameState
loop (x, y) = do
    event <- waitEvent
    newGameState <- case event of
        Quit -> exitWith ExitSuccess
        KeyDown (Keysym _ _ 'q') -> exitWith ExitSuccess
        KeyDown (Keysym _ _ 'h') -> return (x - 10, y)
        KeyDown (Keysym _ _ 'j') -> return (x, y + 10)
        KeyDown (Keysym _ _ 'k') -> return (x, y - 10)
        KeyDown (Keysym _ _ 'l') -> return (x + 10, y)
        _ -> return (x, y)
    display newGameState
    loop newGameState
