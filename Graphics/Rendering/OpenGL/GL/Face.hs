-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Face
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- This is a purely internal module for (un-)marshaling Face.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Face (
   Face(..), marshalFace, unmarshalFace
) where

import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLenum )

--------------------------------------------------------------------------------

data Face =
     Front
   | Back
   | FrontAndBack
   deriving ( Eq, Ord, Show )

marshalFace :: Face -> GLenum
marshalFace x = case x of
   Front -> 0x404
   Back -> 0x405
   FrontAndBack -> 0x408

unmarshalFace :: GLenum -> Face
unmarshalFace x
   | x == 0x404 = Front
   | x == 0x405 = Back
   | x == 0x408 = FrontAndBack
   | otherwise = error ("unmarshalFace: illegal value " ++ show x)
