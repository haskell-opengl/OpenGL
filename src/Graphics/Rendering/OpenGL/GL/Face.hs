{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Face
-- Copyright   :  (c) Sven Panne 2002-2019
-- License     :  BSD3
-- 
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This is a purely internal module for (un-)marshaling Face.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Face (
   Face(..), marshalFace, unmarshalFace
) where

import Graphics.GL

--------------------------------------------------------------------------------

data Face =
     Front
   | Back
   | FrontAndBack
   deriving ( Eq, Ord, Show )

marshalFace :: Face -> GLenum
marshalFace x = case x of
   Front -> GL_FRONT
   Back -> GL_BACK
   FrontAndBack -> GL_FRONT_AND_BACK

unmarshalFace :: GLenum -> Face
unmarshalFace x
   | x == GL_FRONT = Front
   | x == GL_BACK = Back
   | x == GL_FRONT_AND_BACK = FrontAndBack
   | otherwise = error ("unmarshalFace: illegal value " ++ show x)
