{-# LANGUAGE TemplateHaskell #-}
module Labyrinth where

import Control.Lens
import Control.Monad
import Data.Fixed
import Data.List as L (delete)
import Data.Set as S
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import GraphicsUtils

data Point3D = P3D {
    _x :: Int,
    _y :: Int,
    _z :: Int
} deriving (Eq, Ord, Show, Read)

makeLenses ''Point3D

fromPoint :: Point3D -> (GLfloat, GLfloat, GLfloat)
fromPoint (P3D x y z) = (fromIntegral x, fromIntegral y, fromIntegral z)

data Direction = X | Y | Z | XI | YI | ZI deriving (Eq, Ord, Show, Read)

opposite :: Direction -> Direction
opposite X = XI
opposite Y = YI
opposite Z = ZI
opposite XI = X
opposite YI = Y
opposite ZI = Z

move :: Direction -> Point3D -> Point3D
move X = x %~ (+1)
move Y = y %~ (+1)
move Z = z %~ (+1)
move XI = x %~ subtract 1
move YI = y %~ subtract 1
move ZI = z %~ subtract 1

data Labyrinth = Labyrinth {
    _walls     :: Set (Point3D, Direction),
    _boxes     :: [Point3D],
    _startPos  :: Point3D,
    _finishPos :: Point3D,
    _playerPos :: Point3D
} deriving (Show, Read)

makeLenses ''Labyrinth

emptyLabyrinth :: Labyrinth
emptyLabyrinth = Labyrinth empty [] (P3D 0 0 0) (P3D 0 0 0) (P3D 0 0 0)

toggleWall :: Direction -> Labyrinth -> Labyrinth
toggleWall d l = if canMove l (l ^. playerPos) d
    then toggleWall' insert d
    else toggleWall' S.delete d
  where
    toggleWall' f X = walls %~ f (l ^. playerPos, X) $ l
    toggleWall' f Y = walls %~ f (l ^. playerPos, Y) $ l
    toggleWall' f Z = walls %~ f (l ^. playerPos, Z) $ l
    toggleWall' f XI = walls %~ f (x %~ subtract 1 $ l ^. playerPos, X) $ l
    toggleWall' f YI = walls %~ f (y %~ subtract 1 $ l ^. playerPos, Y) $ l
    toggleWall' f ZI = walls %~ f (z %~ subtract 1 $ l ^. playerPos, Z) $ l

toggleBox :: Labyrinth -> Labyrinth
toggleBox l = if (l ^. playerPos) `elem` l ^. boxes
    then boxes %~ L.delete (l ^. playerPos) $ l
    else boxes %~ ((l ^. playerPos):) $ l

canMove :: Labyrinth -> Point3D -> Direction -> Bool
canMove l p X  = notMember (p, X) (l ^. walls) && not ((move X p) `elem` (l ^. boxes))
canMove l p Y  = notMember (p, Y) (l ^. walls) && not ((move Y p) `elem` (l ^. boxes))
canMove l p Z  = notMember (p, Z) (l ^. walls) && not ((move Z p) `elem` (l ^. boxes))
canMove l p XI = notMember ((x %~ subtract 1 $ p), X) (l ^. walls)
    && not ((move XI p) `elem` (l ^. boxes))
canMove l p YI = notMember ((y %~ subtract 1 $ p), Y) (l ^. walls)
    && not ((move YI p) `elem` (l ^. boxes))
canMove l p ZI = notMember ((z %~ subtract 1 $ p), Z) (l ^. walls)
    && not ((move ZI p) `elem` (l ^. boxes))

movePlayer :: Direction -> Labyrinth -> Labyrinth
movePlayer X l = playerPos . x %~ (+1) $ l
movePlayer Y l = playerPos . y %~ (+1) $ l
movePlayer Z l = playerPos . z %~ (+1) $ l
movePlayer XI l = playerPos . x %~ subtract 1 $ l
movePlayer YI l = playerPos . y %~ subtract 1 $ l
movePlayer ZI l = playerPos . z %~ subtract 1 $ l

tryMovePlayer :: Direction -> Labyrinth -> Labyrinth
tryMovePlayer d l = if canMove l (l ^. playerPos) d then movePlayer d l else l

moveBoxes :: Direction -> Labyrinth -> Labyrinth
moveBoxes d l = boxes %~ fmap (\x -> if canMove l x d && move d x /= l ^. playerPos
    then move d x
    else x) $ l

w1c = Color4 0.7 0.7 0.7 1
w2c = Color4 0.8 0.8 0.8 1
w3c = Color4 0.9 0.9 0.9 1

renderWall :: (GLfloat, GLfloat, GLfloat) -> Direction -> IO ()
renderWall (x, y, z) X = renderBox (x + 0.9) y z (x + 1.1) (y + 1) (z + 1) w1c w2c w3c
renderWall (x, y, z) Y = renderBox x (y + 0.9) z (x + 1) (y + 1.1) (z + 1) w1c w2c w3c
renderWall (x, y, z) Z = renderBox x y (z + 0.9) (x + 1) (y + 1) (z + 1.1) w1c w2c w3c

renderLabyrinth :: Labyrinth -> IO ()
renderLabyrinth l = do
    forM_ (l ^. walls) $ \(p, d) -> renderWall (fromPoint p) d
    forM_ (l ^. boxes) $ \p -> let (x, y, z) = fromPoint p
        in renderBox (x + 0.1) (y + 0.1) (z + 0.1) (x + 0.9) (y + 0.9) (z + 0.9)
            (Color4 0.3 0.3 0.3 1) (Color4 0.4 0.4 0.4 1) (Color4 0.5 0.5 0.5 1)
    let (x, y, z) = fromPoint $ l ^. startPos
    renderBox (x + 0.11) (y + 0.11) (z + 0.11) (x + 0.89) (y + 0.89) (z + 0.89)
        (Color4 0 0.7 0 1) (Color4 0 0.8 0 1) (Color4 0 0.9 0 1)
    let (x, y, z) = fromPoint $ l ^. finishPos
    renderBox (x + 0.08) (y + 0.08) (z + 0.08) (x + 0.92) (y + 0.92) (z + 0.92)
        (Color4 0.7 0 0 1) (Color4 0.8 0 0 1) (Color4 0.9 0 0 1)
    let (x, y, z) = fromPoint $ l ^. playerPos
    renderBox (x + 0.1) (y + 0.1) (z + 0.1) (x + 0.9) (y + 0.9) (z + 0.9)
        (Color4 0.7 0.7 0 1) (Color4 0.8 0.8 0 1) (Color4 0.9 0.9 0 5)
    currentColor $= Color4 1 0 0 1
    renderPrimitive Quads $ mapM_ toVertex [(x + 0.09, y + 0.4, z + 0.4),
        (x + 0.09, y + 0.4, z + 0.6), (x + 0.09, y + 0.6, z + 0.6),
        (x + 0.09, y + 0.6, z + 0.4), (x + 0.91, y + 0.4, z + 0.4),
        (x + 0.91, y + 0.4, z + 0.6), (x + 0.91, y + 0.6, z + 0.6),
        (x + 0.91, y + 0.6, z + 0.4)]
    currentColor $= Color4 0 1 0 1
    renderPrimitive Quads $ mapM_ toVertex [(x + 0.4, y + 0.09, z + 0.4),
        (x + 0.4, y + 0.09, z + 0.6), (x + 0.6, y + 0.09, z + 0.6),
        (x + 0.6, y + 0.09, z + 0.4), (x + 0.4, y + 0.91, z + 0.4),
        (x + 0.4, y + 0.91, z + 0.6), (x + 0.6, y + 0.91, z + 0.6),
        (x + 0.6, y + 0.91, z + 0.4)]
    currentColor $= Color4 0 0 1 1
    renderPrimitive Quads $ mapM_ toVertex [(x + 0.4, y + 0.4, z + 0.09),
        (x + 0.4, y + 0.6, z + 0.09), (x + 0.6, y + 0.6, z + 0.09),
        (x + 0.6, y + 0.4, z + 0.09), (x + 0.4, y + 0.4, z + 0.91),
        (x + 0.4, y + 0.6, z + 0.91), (x + 0.6, y + 0.6, z + 0.91),
        (x + 0.6, y + 0.4, z + 0.91)]
    currentColor $= Color4 0 0 0 1
    renderPrimitive Quads $ mapM_ toVertex [(x + 0.915, y + 0.45, z + 0.45),
        (x + 0.915, y + 0.45, z + 0.55), (x + 0.915, y + 0.55, z + 0.55),
        (x + 0.915, y + 0.55, z + 0.45), (x + 0.45, y + 0.915, z + 0.45),
        (x + 0.45, y + 0.915, z + 0.55), (x + 0.55, y + 0.915, z + 0.55),
        (x + 0.55, y + 0.915, z + 0.45), (x + 0.45, y + 0.45, z + 0.915),
        (x + 0.45, y + 0.55, z + 0.915), (x + 0.55, y + 0.55, z + 0.915),
        (x + 0.55, y + 0.45, z + 0.915)]
