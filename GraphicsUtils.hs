module GraphicsUtils where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

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

toVertex :: (GLfloat, GLfloat, GLfloat) -> IO ()
toVertex (x, y, z) = vertex $ Vertex3 x y z

renderBox :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> 
    Color4 GLfloat -> Color4 GLfloat -> Color4 GLfloat -> IO ()
renderBox x y z x' y' z' c1 c2 c3 = do
    currentColor $= c1
    renderPrimitive Quads $ mapM_ toVertex [(x, y, z), (x, y, z'), (x, y', z'), (x, y', z),
        (x', y, z), (x', y, z'), (x', y', z'), (x', y', z)]
    currentColor $= c2
    renderPrimitive Quads $ mapM_ toVertex [(x, y, z), (x, y, z'), (x', y, z'), (x', y, z),
        (x, y', z), (x, y', z'), (x', y', z'), (x', y', z)]
    currentColor $= c3
    renderPrimitive Quads $ mapM_ toVertex [(x, y, z), (x, y', z), (x', y', z), (x', y, z),
        (x, y, z'), (x, y', z'), (x', y', z'), (x', y, z')]
