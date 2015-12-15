{-# LANGUAGE TemplateHaskell #-}
module Labyrinth where

import Control.Lens
import Data.Fixed
import Data.Set
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import GraphicsUtils

data Point3D = P3D {
    _x :: Int,
    _y :: Int,
    _z :: Int
} deriving (Eq, Ord, Show, Read)

makeLenses ''Point3D

data Direction = X | Y | Z | XI | YI | ZI deriving (Eq, Ord, Show, Read)

opposite :: Direction -> Direction
opposite X = XI
opposite Y = YI
opposite Z = ZI
opposite XI = X
opposite YI = Y
opposite ZI = Z

data Labyrinth = Labyrinth {
    _walls     :: Set (Point3D, Direction),
    _startPos  :: Point3D,
    _finishPos :: Point3D,
    _playerPos :: Point3D
} deriving (Show, Read)

makeLenses ''Labyrinth

emptyLabyrinth :: Labyrinth
emptyLabyrinth = Labyrinth empty (P3D 0 0 0) (P3D 0 0 0) (P3D 0 0 0)

initialLabyrinth :: Labyrinth
initialLabyrinth = Labyrinth initialWalls (P3D (-3) 0 0) (P3D (-2) 2 1) (P3D (-3) 0 0)
  where
    initialWalls = insert (P3D (-3) 0 0, X) $ insert (P3D 0 1 0, X) $ insert (P3D 0 2 0, X) $ insert (P3D 0 2 0, Z) $ insert (P3D (-1) 2 0, Z) empty

toggleWall :: Direction -> Labyrinth -> Labyrinth
toggleWall d l = if canMove l (l ^. playerPos) d
    then toggleWall' insert d
    else toggleWall' delete d
  where
    toggleWall' f X = walls %~ f (l ^. playerPos, X) $ l
    toggleWall' f Y = walls %~ f (l ^. playerPos, Y) $ l
    toggleWall' f Z = walls %~ f (l ^. playerPos, Z) $ l
    toggleWall' f XI = walls %~ f (x %~ subtract 1 $ l ^. playerPos, X) $ l
    toggleWall' f YI = walls %~ f (y %~ subtract 1 $ l ^. playerPos, Y) $ l
    toggleWall' f ZI = walls %~ f (z %~ subtract 1 $ l ^. playerPos, Z) $ l

canMove :: Labyrinth -> Point3D -> Direction -> Bool
canMove l p X  = notMember (p, X) $ l ^. walls
canMove l p Y  = notMember (p, Y) $ l ^. walls
canMove l p Z  = notMember (p, Z) $ l ^. walls
canMove l p XI = notMember ((x %~ subtract 1 $ p), X) $ l ^. walls
canMove l p YI = notMember ((y %~ subtract 1 $ p), Y) $ l ^. walls
canMove l p ZI = notMember ((z %~ subtract 1 $ p), Z) $ l ^. walls

movePlayer :: Direction -> Labyrinth -> Labyrinth
movePlayer X l = playerPos . x %~ (+1) $ l
movePlayer Y l = playerPos . y %~ (+1) $ l
movePlayer Z l = playerPos . z %~ (+1) $ l
movePlayer XI l = playerPos . x %~ subtract 1 $ l
movePlayer YI l = playerPos . y %~ subtract 1 $ l
movePlayer ZI l = playerPos . z %~ subtract 1 $ l

tryMovePlayer :: Direction -> Labyrinth -> Labyrinth
tryMovePlayer d l = if canMove l (l ^. playerPos) d then movePlayer d l else l

w1c = Color4 0.7 0.7 0.7 1
w2c = Color4 0.8 0.8 0.8 1
w3c = Color4 0.9 0.9 0.9 1

fromPoint :: Point3D -> (GLfloat, GLfloat, GLfloat)
fromPoint (P3D x y z) = (fromIntegral x, fromIntegral y, fromIntegral z)

renderWall :: (GLfloat, GLfloat, GLfloat) -> Direction -> IO ()
renderWall (x, y, z) X = renderBox (x + 0.9) y z (x + 1.1) (y + 1) (z + 1) w1c w2c w3c
renderWall (x, y, z) Y = renderBox x (y + 0.9) z (x + 1) (y + 1.1) (z + 1) w1c w2c w3c
renderWall (x, y, z) Z = renderBox x y (z + 0.9) (x + 1) (y + 1) (z + 1.1) w1c w2c w3c

renderLabyrinth :: Labyrinth -> IO ()
renderLabyrinth l = do
    mapM_ (\(p, d) -> renderWall (fromPoint p) d) $ l ^. walls
    let (x, y, z) = fromPoint $ l ^. startPos
    renderBox (x + 0.1) (y + 0.1) (z + 0.1) (x + 0.9) (y + 0.9) (z + 0.9)
        (Color4 0 0.7 0 1) (Color4 0 0.8 0 1) (Color4 0 0.9 0 1)
    let (x, y, z) = fromPoint $ l ^. finishPos
    renderBox (x + 0.1) (y + 0.1) (z + 0.1) (x + 0.9) (y + 0.9) (z + 0.9)
        (Color4 0.7 0 0 1) (Color4 0.8 0 0 1) (Color4 0.9 0 0 1)
    let (x, y, z) = fromPoint $ l ^. playerPos
    renderBox (x + 0.1) (y + 0.1) (z + 0.1) (x + 0.9) (y + 0.9) (z + 0.9)
        (Color4 0.7 0.7 0 1) (Color4 0.8 0.8 0 1) (Color4 0.9 0.9 0 5)
