module Main where

import Data.IORef
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import System.Environment

import EditorState

reshape :: Size -> IO ()
reshape s@(Size w h) = do
    viewport   $= (Position 0 0, s)
    matrixMode $= Projection
    loadIdentity
    let near   = 0.001
        far    = 40
        fov    = 90
        ang    = (fov * pi) / 360
        top    = near / (cos ang / sin ang)
        aspect = fromIntegral w / fromIntegral h
        right  = top * aspect
    frustum (-right) right (-top) top near far
    matrixMode $= Modelview 0

main :: IO ()
main = do
    args <- getArgs
    if length args /= 1 then putStrLn "Result filename expected as an argument."
    else do
        getArgsAndInitialize
        initialDisplayMode $= [DoubleBuffered, WithDepthBuffer]
        createWindow "L3Editor"

        state <- newIORef $ initialEditor $ head args

        fullScreen
        displayCallback       $= renderState state
        reshapeCallback       $= Just reshape
        keyboardMouseCallback $= Just (inputHandler state)
        depthFunc             $= Just Less

        mainLoop
