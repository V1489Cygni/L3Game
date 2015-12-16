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
    _pov       :: Point3D,
    _counter   :: Int,
    _initLab   :: Labyrinth
}

makeLenses ''GameState

loadGame :: String -> GameState
loadGame s = let l = read s
    in GameState l X initialCameraState (l ^. playerPos) 0 (read s)

renderGameState :: IORef GameState -> IO ()
renderGameState state = do
    gs <- readIORef state
    renderState (gs ^. labyrinth) (gs ^. direction) (gs ^. camera) (gs ^. pov)

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
        let finished = gs ^. labyrinth . playerPos == gs^. labyrinth . finishPos
        case modifiers of
            Modifiers Up Up Up -> if isFalling gs && not finished then return ()
                else do
                    case key of
                        Char 'x' -> do
                            modifyIORef state $ (direction .~ X) . (counter .~ 0)
                            modifyIORef state $ labyrinth %~ moveBoxes (gs ^. direction)
                        Char 'y' -> do
                            modifyIORef state $ (direction .~ Y) . (counter .~ 0)
                            modifyIORef state $ labyrinth %~ moveBoxes (gs ^. direction)
                        Char 'z' -> do
                            modifyIORef state $ (direction .~ Z) . (counter .~ 0)
                            modifyIORef state $ labyrinth %~ moveBoxes (gs ^. direction)
                        Char ' ' -> do
                            modifyIORef state $ labyrinth %~ moveBoxes (gs ^. direction)
                            modifyIORef state $ counter .~ 0
                            modifyIORef state $ \s -> pov .~ (s ^. labyrinth . playerPos) $ s
                        Char 'w' -> modifyIORef state $ pov %~ move (getDirection (gs ^. direction) KeyUp)
                        Char 's' -> modifyIORef state $ pov %~ move (getDirection (gs ^. direction) KeyDown)
                        Char 'a' -> modifyIORef state $ pov %~ move (getDirection (gs ^. direction) KeyLeft)
                        Char 'd' -> modifyIORef state $ pov %~ move (getDirection (gs ^. direction) KeyRight)
                        SpecialKey k -> if elem k [KeyUp, KeyDown, KeyLeft, KeyRight] && not finished
                            then do
                                modifyIORef state $ (moveByKey k) . (counter .~ 0)
                                modifyIORef state $ labyrinth %~ moveBoxes (gs ^. direction)
                                modifyIORef state $ \s -> pov .~ (s ^. labyrinth . playerPos) $ s
                            else return ()
                        _ -> return ()
            Modifiers Down Up Up -> if isFalling gs then return ()
                else do
                    case key of
                        Char 'X' -> modifyIORef state $ (direction .~ XI) . (counter .~ 0)
                        Char 'Y' -> modifyIORef state $ (direction .~ YI) . (counter .~ 0)
                        Char 'Z' -> modifyIORef state $ (direction .~ ZI) . (counter .~ 0)
                        Char '<' -> modifyIORef state $ pov %~ move (opposite $ gs ^. direction)
                        Char '>' -> modifyIORef state $ pov %~ move (gs ^. direction)
                        _        -> return ()
            Modifiers Up Down Up -> case key of
                Char '\DC1' -> exit >> exitSuccess
                Char '\DC2' -> do
                    modifyIORef state $ labyrinth .~ (gs ^. initLab)
                    modifyIORef state $ (direction .~ X) . (counter .~ 0)
                    modifyIORef state $ pov .~ (gs ^. labyrinth . startPos)
                _           -> return ()
            _ -> return ()
    postRedisplay Nothing

idleHandler :: IORef GameState -> IO ()
idleHandler state = do
    gs <- readIORef state
    if gs ^. labyrinth . playerPos == gs ^. labyrinth . finishPos then return ()
    else do
        if isFalling gs
            then if gs ^. counter == 25
                then let gs2 = counter .~ 0 $ gs
                         gs3 = labyrinth %~ tryMovePlayer (gs ^. direction) $ gs2
                         gs4 = labyrinth %~ moveBoxes (gs ^. direction) $ gs3
                         gs5 = pov .~ (gs4 ^. labyrinth . playerPos) $ gs4
                     in writeIORef state gs5
                else modifyIORef state $ counter %~ (+1)
            else return ()
    postRedisplay Nothing
