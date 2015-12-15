{-# LANGUAGE TemplateHaskell #-}
module EditorState where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Control.Lens
import Data.IORef
import Data.Fixed
import System.Exit

import Labyrinth
import StateUtils

data EditorState = EditorState {
    _labyrinth :: Labyrinth,
    _direction :: Direction,
    _camera    :: CameraState,
    _fileName  :: String
}

makeLenses ''EditorState

initialEditorState :: String -> EditorState
initialEditorState = EditorState emptyLabyrinth X initialCameraState

loadEditorState :: String -> String -> EditorState
loadEditorState contents = EditorState (read contents) X initialCameraState

renderEditorState :: IORef EditorState -> IO ()
renderEditorState state = do
    es <- readIORef state
    renderState (es ^. labyrinth) (es ^. direction) (es ^. camera)

moveByKey :: SpecialKey -> EditorState -> EditorState
moveByKey key es = labyrinth %~ movePlayer (getDirection (es ^. direction) key) $ es

inputHandler :: IORef EditorState -> KeyboardMouseCallback
inputHandler state key keyState modifiers _ = do
    if keyState /= Down then return ()
    else do
        modifyIORef state $ camera %~ cameraInputHandler key modifiers
        es <- readIORef state
        case modifiers of
            Modifiers Up Up Up -> case key of
                Char 'x' -> modifyIORef state $ labyrinth %~ toggleWall X
                Char 'y' -> modifyIORef state $ labyrinth %~ toggleWall Y
                Char 'z' -> modifyIORef state $ labyrinth %~ toggleWall Z
                Char 's' -> modifyIORef state $ labyrinth . startPos .~ player es
                Char 'f' -> modifyIORef state $ labyrinth . finishPos .~ player es
                Char 'b' -> modifyIORef state $ labyrinth %~ toggleBox
                SpecialKey k -> if k `elem` [KeyUp, KeyDown, KeyLeft, KeyRight]
                    then modifyIORef state $ moveByKey k
                    else return ()
                _ -> return ()
            Modifiers Down Up Up -> case key of
                Char 'X' -> modifyIORef state $ labyrinth %~ toggleWall XI
                Char 'Y' -> modifyIORef state $ labyrinth %~ toggleWall YI
                Char 'Z' -> modifyIORef state $ labyrinth %~ toggleWall ZI
                Char '<' -> modifyIORef state $ labyrinth %~
                    movePlayer (opposite (es ^. direction))
                Char '>' -> modifyIORef state $ labyrinth %~ movePlayer (es ^. direction)
                _        -> return ()
            Modifiers Up Down Up -> case key of
                Char '\DC1' -> exit >> exitSuccess
                Char '\DC3' -> writeFile (es ^. fileName) $ show $ 
                    playerPos .~ (es ^. labyrinth . startPos) $ es ^. labyrinth
                Char '\SUB' -> modifyIORef state $ direction .~ Z
                Char '\CAN' -> modifyIORef state $ direction .~ X
                Char '\EM'  -> modifyIORef state $ direction .~ Y
                _           -> return ()
            Modifiers Down Down Up -> case key of
                Char '\SUB' -> modifyIORef state $ direction .~ ZI
                Char '\CAN' -> modifyIORef state $ direction .~ XI
                Char '\EM'  -> modifyIORef state $ direction .~ YI
                _           -> return ()
            _ -> return ()
    postRedisplay Nothing
  where
    player es = es ^. labyrinth . playerPos
