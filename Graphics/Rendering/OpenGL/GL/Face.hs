-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Face
-- Copyright   :  (c) Sven Panne 2002-2009
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  stable
-- Portability :  portable
--
-- This is a purely internal module for (un-)marshaling Face.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Face (
   Face(..), marshalFace, unmarshalFace
) where

import Graphics.Rendering.OpenGL.Raw.Core31

--------------------------------------------------------------------------------

data Face =
     Front
   | Back
   | FrontAndBack
   deriving ( Eq, Ord, Show )

marshalFace :: Face -> GLenum
marshalFace x = case x of
   Front -> gl_FRONT
   Back -> gl_BACK
   FrontAndBack -> gl_FRONT_AND_BACK

unmarshalFace :: GLenum -> Face
unmarshalFace x
   | x == gl_FRONT = Front
   | x == gl_BACK = Back
   | x == gl_FRONT_AND_BACK = FrontAndBack
   | otherwise = error ("unmarshalFace: illegal value " ++ show x)
