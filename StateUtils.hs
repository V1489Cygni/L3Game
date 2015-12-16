{-# LANGUAGE TemplateHaskell #-}
module StateUtils where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Control.Lens hiding (filtered)
import Data.IORef
import Data.Fixed
import System.Exit

import Labyrinth

data CameraState = CameraState {
    _alpha     :: GLfloat,
    _beta      :: GLfloat,
    _distance  :: GLfloat,
    _filtered  :: Bool,
    _filterAll :: Bool,
    _showXDown :: Bool
}

makeLenses ''CameraState

initialCameraState :: CameraState
initialCameraState = CameraState 45 0 3.2 False False False

getFilter :: Direction -> Point3D -> (Point3D, Direction) -> Bool
getFilter X p1 (p2, d) = p1 ^. x <= p2 ^. x
getFilter Y p1 (p2, d) = p1 ^. y <= p2 ^. y
getFilter Z p1 (p2, d) = p1 ^. z <= p2 ^. z
getFilter XI p1 (p2, d) = p1 ^. x >= p2 ^. x + if d == X then 1 else 0
getFilter YI p1 (p2, d) = p1 ^. y >= p2 ^. y + if d == Y then 1 else 0
getFilter ZI p1 (p2, d) = p1 ^. z >= p2 ^. z + if d == Z then 1 else 0

renderState :: Labyrinth -> Direction -> CameraState -> Point3D -> IO ()
renderState labyrinth direction state pov = do
    clear [ColorBuffer, DepthBuffer]
    loadIdentity
    translate $ Vector3 (0 :: GLfloat) 0 $ (-state ^. distance)
    rotate (state ^. alpha - 90) $ Vector3 (1 :: GLfloat) 0 0
    rotate (state ^. beta)  $ Vector3 (0 :: GLfloat) 0 1
    if state ^. showXDown
        then do
            rotate  90 $ Vector3 (0 :: GLfloat) 1 0
            rotate 270 $ Vector3 (1 :: GLfloat) 0 0
        else case direction of
            X  -> do
                rotate  90 $ Vector3 (0 :: GLfloat) 1 0
                rotate 270 $ Vector3 (1 :: GLfloat) 0 0
            Y  -> do
                rotate 270 $ Vector3 (0 :: GLfloat) 1 0
                rotate  90 $ Vector3 (0 :: GLfloat) 0 1
            Z  -> do
                rotate 180 $ Vector3 (0 :: GLfloat) 1 0
            XI -> do
                rotate  90 $ Vector3 (0 :: GLfloat) 1 0
                rotate 270 $ Vector3 (1 :: GLfloat) 0 0
                rotate 180 $ Vector3 (0 :: GLfloat) 1 0
            YI -> do
                rotate 270 $ Vector3 (0 :: GLfloat) 1 0
                rotate 270 $ Vector3 (0 :: GLfloat) 0 1
            ZI -> do
                rotate 180 $ Vector3 (0 :: GLfloat) 1 0
                rotate 180 $ Vector3 (1 :: GLfloat) 0 0
    let (x, y, z) = fromPoint pov
    translate $ Vector3 (-0.5 - x) (-0.5 - y) (-0.5 - z)
    renderLabyrinth (if state ^. filterAll then const False
        else if state ^. filtered
            then getFilter direction pov
            else const True) labyrinth
    flush
    swapBuffers

getDirection' :: [Direction] -> SpecialKey -> Direction
getDirection' d k = d !! offset k
  where
    offset KeyUp    = 0
    offset KeyDown  = 1
    offset KeyLeft  = 2
    offset KeyRight = 3

getDirection :: Direction -> SpecialKey -> Direction
getDirection X = getDirection' [Z, ZI, Y, YI]
getDirection Y = getDirection' [X, XI, Z, ZI]
getDirection Z = getDirection' [Y, YI, X, XI]
getDirection XI = getDirection' [ZI, Z, Y, YI]
getDirection YI = getDirection' [XI, X, Z, ZI]
getDirection ZI = getDirection' [YI, Y, X, XI]

cameraInputHandler :: Key -> Modifiers -> CameraState -> CameraState
cameraInputHandler key modifiers = if modifiers == Modifiers Up Down Up
    then case key of
        SpecialKey KeyUp    -> alpha %~ min 90 . (+2)
        SpecialKey KeyDown  -> alpha %~ max (-90) . subtract 2
        SpecialKey KeyLeft  -> beta %~ flip mod' 360 . (+2)
        SpecialKey KeyRight -> beta %~ flip mod' 360 . (+358)
        Char '+'            -> distance %~ max 1 . subtract 0.2
        Char '-'            -> distance %~ max 1 . (+0.2)
        Char '\t'           -> const initialCameraState
        Char '\ACK'         -> (filtered %~ not) . (filterAll .~ False)
        Char '\a'           -> (filterAll %~ not) . (filtered .~ False)
        Char '\EOT'         -> showXDown %~ not
        _                   -> id
    else id
