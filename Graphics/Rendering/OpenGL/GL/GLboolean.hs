-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.GLboolean
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- This is a purely internal module for (un-)marshaling GLboolean.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.GLboolean (
   GLboolean, marshalGLboolean, unmarshalGLboolean
) where

import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLboolean )

--------------------------------------------------------------------------------

marshalGLboolean :: Bool -> GLboolean
marshalGLboolean False = 0
marshalGLboolean True  = 1

unmarshalGLboolean :: GLboolean -> Bool
unmarshalGLboolean = (/= 0)
