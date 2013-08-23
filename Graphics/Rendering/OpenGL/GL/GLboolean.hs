-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.GLboolean
-- Copyright   :  (c) Sven Panne 2002-2013
-- License     :  BSD3
-- 
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This is a purely internal module for (un-)marshaling GLboolean.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.GLboolean (
   marshalGLboolean, unmarshalGLboolean
) where

import Graphics.Rendering.OpenGL.Raw.Core31

--------------------------------------------------------------------------------

marshalGLboolean :: Num a => Bool -> a
marshalGLboolean x = fromIntegral $ case x of
   False -> gl_FALSE
   True -> gl_TRUE

unmarshalGLboolean :: (Eq a, Num a) => a -> Bool
unmarshalGLboolean = (/= fromIntegral gl_FALSE)
