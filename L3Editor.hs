module Main where

import Data.IORef
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import System.Directory
import System.Environment

import EditorState
import GraphicsUtils

main :: IO ()
main = do
    args <- getArgs
    if length args /= 1 then putStrLn "Filename expected as an argument."
    else do
        getArgsAndInitialize
        initialDisplayMode $= [DoubleBuffered, WithDepthBuffer]
        createWindow "L3Editor"

        let fileName = head args
        b <- doesFileExist $ fileName
        es <- if b then (\c -> loadEditorState c fileName) <$> readFile fileName
                   else return $ initialEditorState fileName
        state <- newIORef es

        fullScreen
        displayCallback       $= renderEditorState state
        reshapeCallback       $= Just reshape
        keyboardMouseCallback $= Just (inputHandler state)
        depthFunc             $= Just Less

        mainLoop
