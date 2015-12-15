{-# LANGUAGE TemplateHaskell #-}
module GameState where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Control.Lens
import Data.IORef
import Data.Fixed
import System.Exit

import Labyrinth
import StateUtils

data GameState = GameState {
    _labyrinth :: Labyrinth,
    _direction :: Direction,
    _camera    :: CameraState,
    _counter   :: Int
}

makeLenses ''GameState

loadGame :: String -> GameState
loadGame s = GameState (read s) X initialCameraState 0

renderGameState :: IORef GameState -> IO ()
renderGameState state = do
    gs <- readIORef state
    renderState (gs ^. labyrinth) (gs ^. direction) (gs ^. camera)

isFalling :: GameState -> Bool
isFalling s = canMove (s ^. labyrinth) (s ^. labyrinth . playerPos) (s ^. direction)

moveByKey :: SpecialKey -> GameState -> GameState
moveByKey key gs = labyrinth %~ tryMovePlayer (getDirection (gs ^. direction) key) $ gs

inputHandler :: IORef GameState -> KeyboardMouseCallback
inputHandler state key keyState modifiers _ = do
    if keyState /= Down then return ()
    else do
        modifyIORef state $ camera %~ cameraInputHandler key modifiers
        gs <- readIORef state
        case modifiers of
            Modifiers Up Up Up -> if isFalling gs then return ()
                else do
                    case key of
                        Char 'x' -> modifyIORef state $ direction .~ X
                        Char 'y' -> modifyIORef state $ direction .~ Y
                        Char 'z' -> modifyIORef state $ direction .~ Z
                        SpecialKey k -> if k `elem` [KeyUp, KeyDown, KeyLeft, KeyRight]
                            then modifyIORef state $ moveByKey k
                            else return ()
                        _                   -> return ()
                    modifyIORef state $ counter .~ 0
            Modifiers Down Up Up -> if isFalling gs then return ()
                else do
                    case key of
                        Char 'X' -> modifyIORef state $ direction .~ XI
                        Char 'Y' -> modifyIORef state $ direction .~ YI
                        Char 'Z' -> modifyIORef state $ direction .~ ZI
                        _        -> return ()
                    modifyIORef state $ counter .~ 0
            Modifiers Up Down Up -> case key of
                Char '\DC1' -> exit >> exitSuccess
                _           -> return ()
            _                    -> return ()
    postRedisplay Nothing

idleHandler :: IORef GameState -> IO ()
idleHandler state = do
    gs <- readIORef state
    if gs ^. labyrinth . playerPos == gs ^. labyrinth . finishPos then do
        putStrLn "A winner is you!"
        exit
        exitSuccess
    else do
        if isFalling gs
            then if gs ^. counter == 25
                then let gs2 = counter .~ 0 $ gs
                         gs3 = labyrinth %~ tryMovePlayer (gs ^. direction) $ gs2
                     in writeIORef state gs3
                else modifyIORef state $ counter %~ (+1)
            else return ()
        postRedisplay Nothing
