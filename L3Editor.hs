module Main where

import Data.IORef
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import System.Environment

import EditorState
import GraphicsUtils

main :: IO ()
main = do
    args <- getArgs
    if length args /= 1 then putStrLn "Result filename expected as an argument."
    else do
        getArgsAndInitialize
        initialDisplayMode $= [DoubleBuffered, WithDepthBuffer]
        createWindow "L3Editor"

        state <- newIORef $ initialEditorState $ head args

        fullScreen
        displayCallback       $= renderEditorState state
        reshapeCallback       $= Just reshape
        keyboardMouseCallback $= Just (inputHandler state)
        depthFunc             $= Just Less

        mainLoop
