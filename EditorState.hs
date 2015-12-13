{-# LANGUAGE TemplateHaskell #-}
module EditorState where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Control.Lens
import Data.IORef
import Data.Fixed
import System.Exit

import Labyrinth

data EditorState = EditorState {
    _labyrinth :: Labyrinth,
    _direction :: Direction,
    _alpha     :: GLfloat,
    _beta      :: GLfloat,
    _distance  :: GLfloat,
    _fileName  :: String
}

makeLenses ''EditorState

initialEditor :: String -> EditorState
initialEditor = EditorState emptyLabyrinth X 45 90 3

renderState :: IORef EditorState -> IO ()
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

inputHandler :: IORef EditorState -> KeyboardMouseCallback
inputHandler state key ks m p = do
    gs <- readIORef state
    if ks == Down then case m of
        Modifiers Up Up Up -> case key of
            Char 'x'            -> writeIORef state (labyrinth %~ toggleWall X $ gs)
            Char 'y'            -> writeIORef state (labyrinth %~ toggleWall Y $ gs)
            Char 'z'            -> writeIORef state (labyrinth %~ toggleWall Z $ gs)
            Char 's'            ->
                writeIORef state (labyrinth . startPos .~ (gs ^. labyrinth . playerPos) $ gs)
            Char 'f'            ->
                writeIORef state (labyrinth . finishPos .~ (gs ^. labyrinth . playerPos) $ gs)
            SpecialKey KeyUp    -> writeIORef state
                (labyrinth %~ movePlayer (getDirection (gs ^. direction) KeyUp) $ gs)
            SpecialKey KeyDown  -> writeIORef state
                (labyrinth %~ movePlayer (getDirection (gs ^. direction) KeyDown) $ gs)
            SpecialKey KeyLeft  -> writeIORef state
                (labyrinth %~ movePlayer (getDirection (gs ^. direction) KeyLeft) $ gs)
            SpecialKey KeyRight -> writeIORef state
                (labyrinth %~ movePlayer (getDirection (gs ^. direction) KeyRight) $ gs)
            _                   -> return ()
        Modifiers Down Up Up -> case key of
            Char 'X' -> writeIORef state (labyrinth %~ toggleWall XI $ gs)
            Char 'Y' -> writeIORef state (labyrinth %~ toggleWall YI $ gs)
            Char 'Z' -> writeIORef state (labyrinth %~ toggleWall ZI $ gs)
            Char '<' -> writeIORef state (labyrinth %~ movePlayer (gs ^. direction) $ gs)
            Char '>' -> writeIORef state (labyrinth %~ movePlayer (opposite (gs ^. direction)) $ gs)
            _        -> return ()
        Modifiers Up Down Up -> case key of
            SpecialKey KeyUp    -> writeIORef state (alpha    %~ min 90 . (+2)          $ gs)
            SpecialKey KeyDown  -> writeIORef state (alpha    %~ max (-90) . subtract 2 $ gs)
            SpecialKey KeyLeft  -> writeIORef state (beta     %~ flip mod' 360 . (+2)   $ gs)
            SpecialKey KeyRight -> writeIORef state (beta     %~ flip mod' 360 . (+358) $ gs)
            Char '+'            -> writeIORef state (distance %~ max 1 . subtract 0.2   $ gs)
            Char '-'            -> writeIORef state (distance %~ min 50 . (+0.4)        $ gs)
            Char '\DC1'         -> exit >> exitSuccess
            Char '\DC3'         -> writeFile (gs ^. fileName) $ show $ 
                playerPos .~ (gs ^. labyrinth . startPos) $ gs ^. labyrinth
            Char '\SUB'         -> writeIORef state (direction .~ Z $ gs)
            Char '\CAN'         -> writeIORef state (direction .~ X $ gs)
            Char '\EM'          -> writeIORef state (direction .~ Y $ gs)
            _                   -> return ()
        Modifiers Down Down Up -> case key of
            Char '\SUB' -> writeIORef state (direction .~ ZI $ gs)
            Char '\CAN' -> writeIORef state (direction .~ XI $ gs)
            Char '\EM'  -> writeIORef state (direction .~ YI $ gs)
            _           -> return ()
        _                    -> return ()
    else return ()
    postRedisplay Nothing
