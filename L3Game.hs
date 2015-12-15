module Main where

import Data.IORef
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import System.Environment

import GameState
import GraphicsUtils

main :: IO ()
main = do
    args <- getArgs
    if length args /= 1 then putStrLn "Filename expected as an argument."
    else do
        getArgsAndInitialize
        initialDisplayMode $= [DoubleBuffered, WithDepthBuffer]
        createWindow "L3Game"

        level <- readFile $ head args

        state <- newIORef $ loadGame level

        fullScreen
        displayCallback       $= renderGameState state
        reshapeCallback       $= Just reshape
        keyboardMouseCallback $= Just (inputHandler state)
        idleCallback          $= Just (idleHandler state)
        depthFunc             $= Just Less

        mainLoop
