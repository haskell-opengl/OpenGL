-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.BlendingFactor
-- Copyright   :  (c) Sven Panne 2002-2009
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  stable
-- Portability :  portable
--
-- This is a purely internal module for (un-)marshaling BlendingFactor.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.BlendingFactor (
   BlendingFactor(..), marshalBlendingFactor, unmarshalBlendingFactor
) where

import Graphics.Rendering.OpenGL.Raw.Core31

--------------------------------------------------------------------------------

data BlendingFactor =
     Zero
   | One
   | SrcColor
   | OneMinusSrcColor
   | DstColor
   | OneMinusDstColor
   | SrcAlpha
   | OneMinusSrcAlpha
   | DstAlpha
   | OneMinusDstAlpha
   | ConstantColor
   | OneMinusConstantColor
   | ConstantAlpha
   | OneMinusConstantAlpha
   | SrcAlphaSaturate
   deriving ( Eq, Ord, Show )

marshalBlendingFactor :: BlendingFactor -> GLenum
marshalBlendingFactor x = case x of
   Zero -> gl_ZERO
   One -> gl_ONE
   SrcColor -> gl_SRC_COLOR
   OneMinusSrcColor -> gl_ONE_MINUS_SRC_COLOR
   DstColor -> gl_DST_COLOR
   OneMinusDstColor -> gl_ONE_MINUS_DST_COLOR
   SrcAlpha -> gl_SRC_ALPHA
   OneMinusSrcAlpha -> gl_ONE_MINUS_SRC_ALPHA
   DstAlpha -> gl_DST_ALPHA
   OneMinusDstAlpha -> gl_ONE_MINUS_DST_ALPHA
   ConstantColor -> gl_CONSTANT_COLOR
   OneMinusConstantColor -> gl_ONE_MINUS_CONSTANT_COLOR
   ConstantAlpha -> gl_CONSTANT_ALPHA
   OneMinusConstantAlpha -> gl_ONE_MINUS_CONSTANT_ALPHA
   SrcAlphaSaturate -> gl_SRC_ALPHA_SATURATE

unmarshalBlendingFactor :: GLenum -> BlendingFactor
unmarshalBlendingFactor x
   | x == gl_ZERO = Zero
   | x == gl_ONE = One
   | x == gl_SRC_COLOR = SrcColor
   | x == gl_ONE_MINUS_SRC_COLOR = OneMinusSrcColor
   | x == gl_DST_COLOR = DstColor
   | x == gl_ONE_MINUS_DST_COLOR = OneMinusDstColor
   | x == gl_SRC_ALPHA = SrcAlpha
   | x == gl_ONE_MINUS_SRC_ALPHA = OneMinusSrcAlpha
   | x == gl_DST_ALPHA = DstAlpha
   | x == gl_ONE_MINUS_DST_ALPHA = OneMinusDstAlpha
   | x == gl_CONSTANT_COLOR = ConstantColor
   | x == gl_ONE_MINUS_CONSTANT_COLOR = OneMinusConstantColor
   | x == gl_CONSTANT_ALPHA = ConstantAlpha
   | x == gl_ONE_MINUS_CONSTANT_ALPHA = OneMinusConstantAlpha
   | x == gl_SRC_ALPHA_SATURATE = SrcAlphaSaturate
   | otherwise = error ("unmarshalBlendingFactor: illegal value " ++ show x)
