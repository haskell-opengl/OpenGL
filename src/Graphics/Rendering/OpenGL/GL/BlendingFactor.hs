{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.BlendingFactor
-- Copyright   :  (c) Sven Panne 2002-2019
-- License     :  BSD3
-- 
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This is a purely internal module for (un-)marshaling BlendingFactor.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.BlendingFactor (
   BlendingFactor(..), marshalBlendingFactor, unmarshalBlendingFactor
) where

import Graphics.GL

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
   Zero -> GL_ZERO
   One -> GL_ONE
   SrcColor -> GL_SRC_COLOR
   OneMinusSrcColor -> GL_ONE_MINUS_SRC_COLOR
   DstColor -> GL_DST_COLOR
   OneMinusDstColor -> GL_ONE_MINUS_DST_COLOR
   SrcAlpha -> GL_SRC_ALPHA
   OneMinusSrcAlpha -> GL_ONE_MINUS_SRC_ALPHA
   DstAlpha -> GL_DST_ALPHA
   OneMinusDstAlpha -> GL_ONE_MINUS_DST_ALPHA
   ConstantColor -> GL_CONSTANT_COLOR
   OneMinusConstantColor -> GL_ONE_MINUS_CONSTANT_COLOR
   ConstantAlpha -> GL_CONSTANT_ALPHA
   OneMinusConstantAlpha -> GL_ONE_MINUS_CONSTANT_ALPHA
   SrcAlphaSaturate -> GL_SRC_ALPHA_SATURATE

unmarshalBlendingFactor :: GLenum -> BlendingFactor
unmarshalBlendingFactor x
   | x == GL_ZERO = Zero
   | x == GL_ONE = One
   | x == GL_SRC_COLOR = SrcColor
   | x == GL_ONE_MINUS_SRC_COLOR = OneMinusSrcColor
   | x == GL_DST_COLOR = DstColor
   | x == GL_ONE_MINUS_DST_COLOR = OneMinusDstColor
   | x == GL_SRC_ALPHA = SrcAlpha
   | x == GL_ONE_MINUS_SRC_ALPHA = OneMinusSrcAlpha
   | x == GL_DST_ALPHA = DstAlpha
   | x == GL_ONE_MINUS_DST_ALPHA = OneMinusDstAlpha
   | x == GL_CONSTANT_COLOR = ConstantColor
   | x == GL_ONE_MINUS_CONSTANT_COLOR = OneMinusConstantColor
   | x == GL_CONSTANT_ALPHA = ConstantAlpha
   | x == GL_ONE_MINUS_CONSTANT_ALPHA = OneMinusConstantAlpha
   | x == GL_SRC_ALPHA_SATURATE = SrcAlphaSaturate
   | otherwise = error ("unmarshalBlendingFactor: illegal value " ++ show x)
