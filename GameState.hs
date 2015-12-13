{-# LANGUAGE TemplateHaskell #-}
module GameState where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Control.Lens
import Data.IORef
import Data.Fixed
import System.Exit

import Labyrinth

data GameState = GameState {
    _labyrinth :: Labyrinth,
    _direction :: Direction,
    _alpha     :: GLfloat,
    _beta      :: GLfloat,
    _distance  :: GLfloat,
    _counter   :: Int
}

makeLenses ''GameState

initialState :: GameState
initialState = GameState initialLabyrinth X 45 90 3 0

renderState :: IORef GameState -> IO ()
renderState state = do
    gs <- readIORef state
    clear [ColorBuffer, DepthBuffer]
    loadIdentity
    translate $ Vector3 (0 :: GLfloat) 0 $ (- gs ^. distance)
    rotate (gs ^. alpha) $ Vector3 (1 :: GLfloat) 0 0
    rotate (gs ^. beta) $ Vector3 (0 :: GLfloat) 1 0
    case gs ^. direction of
        X  -> rotate (-90) $ Vector3 (0 :: GLfloat) 0 1
        Y  -> rotate 180 $ Vector3 (1 :: GLfloat) 0 0
        Z  -> rotate 90 $ Vector3 (1 :: GLfloat) 0 0
        XI -> rotate 90 $ Vector3 (0 :: GLfloat) 0 1
        YI -> return ()
        ZI -> rotate (-90) $ Vector3 (1 :: GLfloat) 0 0
    let (x, y, z) = fromPoint $ gs ^. labyrinth . playerPos
    translate $ Vector3 (-0.5 - x) (-0.5 - y) (-0.5 - z)
    renderLabyrinth $ gs ^. labyrinth
    flush
    swapBuffers

getDirection :: Direction -> SpecialKey -> Direction
getDirection X KeyUp     = Y
getDirection X KeyDown   = YI
getDirection X KeyLeft   = ZI
getDirection X KeyRight  = Z
getDirection Y KeyUp     = X
getDirection Y KeyDown   = XI
getDirection Y KeyLeft   = Z
getDirection Y KeyRight  = ZI
getDirection Z KeyUp     = X
getDirection Z KeyDown   = XI
getDirection Z KeyLeft   = YI
getDirection Z KeyRight  = Y
getDirection XI KeyUp    = YI
getDirection XI KeyDown  = Y
getDirection XI KeyLeft  = ZI
getDirection XI KeyRight = Z
getDirection YI KeyUp    = X
getDirection YI KeyDown  = XI
getDirection YI KeyLeft  = ZI
getDirection YI KeyRight = Z
getDirection ZI KeyUp    = X
getDirection ZI KeyDown  = XI
getDirection ZI KeyLeft  = Y
getDirection ZI KeyRight = YI

isFalling :: GameState -> Bool
isFalling s = canMove (s ^. labyrinth) (s ^. labyrinth . playerPos) (s ^. direction)

inputHandler :: IORef GameState -> KeyboardMouseCallback
inputHandler state key ks m p = do
    gs <- readIORef state
    if ks == Down then case m of
        Modifiers Up Up Up -> if not (isFalling gs) then do
            case key of
                Char 'x'            -> writeIORef state (direction .~ X $ gs)
                Char 'y'            -> writeIORef state (direction .~ Y $ gs)
                Char 'z'            -> writeIORef state (direction .~ Z $ gs)
                SpecialKey KeyUp    -> writeIORef state
                    (labyrinth %~ tryMovePlayer (getDirection (gs ^. direction) KeyUp) $ gs)
                SpecialKey KeyDown  -> writeIORef state
                    (labyrinth %~ tryMovePlayer (getDirection (gs ^. direction) KeyDown) $ gs)
                SpecialKey KeyLeft  -> writeIORef state
                    (labyrinth %~ tryMovePlayer (getDirection (gs ^. direction) KeyLeft) $ gs)
                SpecialKey KeyRight -> writeIORef state
                    (labyrinth %~ tryMovePlayer (getDirection (gs ^. direction) KeyRight) $ gs)
                _                   -> return ()
            modifyIORef state $ counter .~ 0
        else return ()
        Modifiers Down Up Up -> if not (isFalling gs) then do
            case key of
                Char 'X' -> writeIORef state (direction .~ XI $ gs)
                Char 'Y' -> writeIORef state (direction .~ YI $ gs)
                Char 'Z' -> writeIORef state (direction .~ ZI $ gs)
                _        -> return ()
            modifyIORef state $ counter .~ 0
        else return ()
        Modifiers Up Down Up -> case key of
            SpecialKey KeyUp    -> writeIORef state (alpha    %~ min 90 . (+2)          $ gs)
            SpecialKey KeyDown  -> writeIORef state (alpha    %~ max (-90) . subtract 2 $ gs)
            SpecialKey KeyLeft  -> writeIORef state (beta     %~ flip mod' 360 . (+2)   $ gs)
            SpecialKey KeyRight -> writeIORef state (beta     %~ flip mod' 360 . (+358) $ gs)
            Char '+'            -> writeIORef state (distance %~ max 1 . subtract 0.2   $ gs)
            Char '-'            -> writeIORef state (distance %~ min 50 . (+0.4)        $ gs)
            Char '\DC1'         -> exit >> exitSuccess
            _                   -> return ()
        _                    -> return ()
    else return ()
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
