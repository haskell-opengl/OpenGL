--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Rectangles
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- This module corresponds to section 2.9 (Rectangles) of the OpenGL 1.4 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Rectangles (
   Rect(..)
) where

import Data.Int
import Foreign.Ptr ( Ptr )
import Graphics.Rendering.OpenGL.GL.BasicTypes (
   GLshort, GLint, GLfloat, GLdouble )
import Graphics.Rendering.OpenGL.GL.VertexSpec ( Vertex2(..) )
#ifdef __HADDOCK__
import Graphics.Rendering.OpenGL.GL.BeginEnd ( PrimitiveMode(Polygon) )
#endif

--------------------------------------------------------------------------------

#include "HsOpenGLExt.h"
#include "HsOpenGLTypes.h"

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

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "glRects" glRects ::
   GLshort -> GLshort -> GLshort -> GLshort -> IO ()

foreign import CALLCONV unsafe "glRectsv" glRectsv ::
   Ptr GLshort -> Ptr GLshort -> IO ()

instance Rect GLshort_ where
   rect (Vertex2 x1 y1) (Vertex2 x2 y2) = glRects x1 y1 x2 y2
   rectv ptr1 ptr2 = glRectsv ptr1 ptr2

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "glRecti" glRecti ::
   GLint -> GLint -> GLint -> GLint -> IO ()

foreign import CALLCONV unsafe "glRectiv" glRectiv ::
   Ptr GLint -> Ptr GLint -> IO ()

instance Rect GLint_ where
   rect (Vertex2 x1 y1) (Vertex2 x2 y2) = glRecti x1 y1 x2 y2
   rectv ptr1 ptr2 = glRectiv ptr1 ptr2

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "glRectf" glRectf ::
   GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()

foreign import CALLCONV unsafe "glRectfv" glRectfv ::
   Ptr GLfloat -> Ptr GLfloat -> IO ()

instance Rect GLfloat_ where
   rect (Vertex2 x1 y1) (Vertex2 x2 y2) = glRectf x1 y1 x2 y2
   rectv ptr1 ptr2 = glRectfv ptr1 ptr2

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "glRectd" glRectd ::
   GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ()

foreign import CALLCONV unsafe "glRectdv" glRectdv ::
   Ptr GLdouble -> Ptr GLdouble -> IO ()

instance Rect GLdouble_ where
   rect (Vertex2 x1 y1) (Vertex2 x2 y2) = glRectd x1 y1 x2 y2
   rectv ptr1 ptr2 = glRectdv ptr1 ptr2
