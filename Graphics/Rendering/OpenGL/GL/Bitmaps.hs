--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Bitmaps
-- Copyright   :  (c) Sven Panne 2002-2005
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
-- This module corresponds to section 3.7 (Bitmaps) of the OpenGL 1.5 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Bitmaps (
   bitmap
) where

import Foreign.Ptr ( Ptr )
import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLsizei, GLfloat )
import Graphics.Rendering.OpenGL.GL.CoordTrans ( Size(..), Vector2(..) )
import Graphics.Rendering.OpenGL.GL.VertexSpec ( Vertex2(..) )

--------------------------------------------------------------------------------

bitmap :: Size -> (Vertex2 GLfloat) -> (Vector2 GLfloat) -> Ptr a -> IO ()
bitmap (Size w h) (Vertex2 xbo ybo) (Vector2 xbi ybi) =
   glBitmap w h xbo ybo xbi ybi

foreign import CALLCONV unsafe "glBitmap"
   glBitmap :: GLsizei -> GLsizei -> GLfloat -> GLfloat -> GLfloat -> GLfloat
            -> Ptr a -> IO ()
