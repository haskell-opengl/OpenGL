-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.PixelRectangles.Reset
-- Copyright   :  (c) Sven Panne 2002-2005
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
-- This is a purely internal module for (un-)marshaling Reset.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.PixelRectangles.Reset (
   Reset(..), marshalReset
) where

import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLboolean )
import Graphics.Rendering.OpenGL.GL.GLboolean ( marshalGLboolean )

--------------------------------------------------------------------------------

data Reset =
     NoReset
   | Reset
   deriving ( Eq, Ord, Show )

marshalReset :: Reset -> GLboolean
marshalReset x = marshalGLboolean (x == Reset)
