module Graphics.Rendering.OpenGL.GL.BeginEnd where

import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLenum )

data BeginMode =
     Points
   | Lines
   | LineLoop
   | LineStrip
   | Triangles
   | TriangleStrip
   | TriangleFan
   | Quads
   | QuadStrip
   | Polygon
   deriving ( Eq, Ord, Show )

unmarshalBeginMode :: GLenum -> BeginMode
unmarshalBeginMode x
   | x == 0x0 = Points
   | x == 0x1 = Lines
   | x == 0x2 = LineLoop
   | x == 0x3 = LineStrip
   | x == 0x4 = Triangles
   | x == 0x5 = TriangleStrip
   | x == 0x6 = TriangleFan
   | x == 0x7 = Quads
   | x == 0x8 = QuadStrip
   | x == 0x9 = Polygon
   | otherwise = error ("unmarshalBeginMode: illegal value " ++ show x)
