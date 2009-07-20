--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Rectangles
-- Copyright   :  (c) Sven Panne 2002-2009
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to section 2.10 (Rectangles) of the OpenGL 2.1 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Rectangles (
   Rect(..)
) where

import Data.Tensor
import Foreign.C.Types
import Foreign.Ptr
import Graphics.Rendering.OpenGL.Raw.ARB.Compatibility

--------------------------------------------------------------------------------

-- | 'rect' and 'rectv' support efficient specification of rectangles as two
-- corner points. Each rectangle command takes four arguments, organized either
-- as two consecutive pairs of (/x/, /y/) coordinates, or as two pointers to
-- arrays, each containing an (/x/, /y/) pair. The resulting rectangle is
-- defined in the /z/ = 0 plane.
--
-- @'rect' ('Vertex2' x1 y1) ('Vertex2' x2, y2)@ is exactly equivalent to the
-- following sequence:
--
-- @
--    'Graphics.Rendering.OpenGL.GL.BeginEnd.renderPrimitive' 'Graphics.Rendering.OpenGL.GL.BeginEnd.Polygon' $ do
--        'Graphics.Rendering.OpenGL.GL.VertexSpec.vertex' ('Vertex2' x1 y1)
--        'Graphics.Rendering.OpenGL.GL.VertexSpec.vertex' ('Vertex2' x2 y1)
--        'Graphics.Rendering.OpenGL.GL.VertexSpec.vertex' ('Vertex2' x2 y2)
--        'Graphics.Rendering.OpenGL.GL.VertexSpec.vertex' ('Vertex2' x1 y2)
-- @
--
-- Note that if the second vertex is above and to the right of the first vertex,
-- the rectangle is constructed with a counterclockwise winding.

class Rect a where
   rect  :: Vertex2 a -> Vertex2 a -> IO ()
   rectv :: Ptr     a -> Ptr     a -> IO ()

-- GLshort instance
instance Rect CShort where
   rect (Vertex2 x1 y1) (Vertex2 x2 y2) = glRects x1 y1 x2 y2
   rectv ptr1 ptr2 = glRectsv ptr1 ptr2

-- GLint instance
instance Rect CInt where
   rect (Vertex2 x1 y1) (Vertex2 x2 y2) = glRecti x1 y1 x2 y2
   rectv ptr1 ptr2 = glRectiv ptr1 ptr2

-- GLfloat instance
instance Rect CFloat where
   rect (Vertex2 x1 y1) (Vertex2 x2 y2) = glRectf x1 y1 x2 y2
   rectv ptr1 ptr2 = glRectfv ptr1 ptr2

-- GLdouble instance
instance Rect CDouble where
   rect (Vertex2 x1 y1) (Vertex2 x2 y2) = glRectd x1 y1 x2 y2
   rectv ptr1 ptr2 = glRectdv ptr1 ptr2
