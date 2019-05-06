--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Rectangles
-- Copyright   :  (c) Sven Panne 2002-2019
-- License     :  BSD3
-- 
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to section 10.9 (Rectangles) of the OpenGL 4.4 specs.
--
--------------------------------------------------------------------------------

{-# LANGUAGE TypeSynonymInstances #-}

module Graphics.Rendering.OpenGL.GL.Rectangles (
   Rect(..)
) where

import Foreign.Ptr
import Graphics.Rendering.OpenGL.GL.Tensor
import Graphics.GL

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

instance Rect GLshort where
   rect (Vertex2 x1 y1) (Vertex2 x2 y2) = glRects x1 y1 x2 y2
   rectv ptr1 ptr2 = glRectsv ptr1 ptr2

instance Rect GLint where
   rect (Vertex2 x1 y1) (Vertex2 x2 y2) = glRecti x1 y1 x2 y2
   rectv ptr1 ptr2 = glRectiv ptr1 ptr2

instance Rect GLfloat where
   rect (Vertex2 x1 y1) (Vertex2 x2 y2) = glRectf x1 y1 x2 y2
   rectv ptr1 ptr2 = glRectfv ptr1 ptr2

instance Rect GLdouble where
   rect (Vertex2 x1 y1) (Vertex2 x2 y2) = glRectd x1 y1 x2 y2
   rectv ptr1 ptr2 = glRectdv ptr1 ptr2
