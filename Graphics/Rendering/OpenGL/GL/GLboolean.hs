-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.GLboolean
-- Copyright   :  (c) Sven Panne 2002-2005
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
-- This is a purely internal module for (un-)marshaling GLboolean.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.GLboolean (
   marshalGLboolean, unmarshalGLboolean
) where

--------------------------------------------------------------------------------

marshalGLboolean :: Num a => Bool -> a
marshalGLboolean False = 0
marshalGLboolean True  = 1

unmarshalGLboolean :: Num a => a -> Bool
unmarshalGLboolean = (/= 0)
