{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.QueryUtils
-- Copyright   :  (c) Sven Panne 2002-2013
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This is a purely internal module with utilities to query OpenGL state.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.QueryUtils (
   module Graphics.Rendering.OpenGL.GL.QueryUtils.PName,
   module Graphics.Rendering.OpenGL.GL.QueryUtils.VertexAttrib,

   lightIndexToEnum,
   modelviewIndexToEnum, modelviewEnumToIndex,

   maybeNullPtr
) where

import Foreign.Ptr
import Graphics.Rendering.OpenGL.GL.QueryUtils.PName
import Graphics.Rendering.OpenGL.GL.QueryUtils.VertexAttrib
import Graphics.Rendering.OpenGL.Raw.ARB.Compatibility
import Graphics.Rendering.OpenGL.Raw.ARB.VertexBlend
import Graphics.Rendering.OpenGL.Raw.Core32

--------------------------------------------------------------------------------

-- 0x4000 through 0x4FFF are reserved for light numbers

lightIndexToEnum :: GLsizei -> Maybe GLenum
lightIndexToEnum i
   | 0 <= i && i <= maxLightIndex = Just (gl_LIGHT0 + fromIntegral i)
   | otherwise = Nothing

maxLightIndex :: GLsizei
maxLightIndex = 0xFFF

--------------------------------------------------------------------------------

-- 0x1700, 0x850a, and 0x8722 through 0x873f are reserved for modelview matrices

modelviewIndexToEnum :: GLsizei -> Maybe GLenum
modelviewIndexToEnum 0 = Just gl_MODELVIEW
modelviewIndexToEnum 1 = Just gl_MODELVIEW1
modelviewIndexToEnum i
   | 2 <= i && i <= 31 = Just (gl_MODELVIEW2 - 2 + fromIntegral i)
   | otherwise = Nothing

modelviewEnumToIndex :: GLenum -> Maybe GLsizei
modelviewEnumToIndex x
   | x == gl_MODELVIEW = Just 0
   | x == gl_MODELVIEW1 = Just 1
   | gl_MODELVIEW2 <= x && x <= gl_MODELVIEW31 = Just (fromIntegral (x - (gl_MODELVIEW2 - 2)))
   | otherwise = Nothing

--------------------------------------------------------------------------------

maybeNullPtr :: b -> (Ptr a -> b) -> Ptr a -> b
maybeNullPtr n f ptr | ptr == nullPtr = n
                     | otherwise      = f ptr
