-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.BlendingFactor
-- Copyright   :  (c) Sven Panne 2002-2005
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
-- This is a purely internal module for (un-)marshaling BlendingFactor.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.BlendingFactor (
   BlendingFactor(..), marshalBlendingFactor, unmarshalBlendingFactor
) where

import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLenum )

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
   Zero -> 0x0
   One -> 0x1
   SrcColor -> 0x300
   OneMinusSrcColor -> 0x301
   DstColor -> 0x306
   OneMinusDstColor -> 0x307
   SrcAlpha -> 0x302
   OneMinusSrcAlpha -> 0x303
   DstAlpha -> 0x304
   OneMinusDstAlpha -> 0x305
   ConstantColor -> 0x8001
   OneMinusConstantColor -> 0x8002
   ConstantAlpha -> 0x8003
   OneMinusConstantAlpha -> 0x8004
   SrcAlphaSaturate -> 0x308

unmarshalBlendingFactor :: GLenum -> BlendingFactor
unmarshalBlendingFactor x
   | x == 0x0 = Zero
   | x == 0x1 = One
   | x == 0x300 = SrcColor
   | x == 0x301 = OneMinusSrcColor
   | x == 0x306 = DstColor
   | x == 0x307 = OneMinusDstColor
   | x == 0x302 = SrcAlpha
   | x == 0x303 = OneMinusSrcAlpha
   | x == 0x304 = DstAlpha
   | x == 0x305 = OneMinusDstAlpha
   | x == 0x8001 = ConstantColor
   | x == 0x8002 = OneMinusConstantColor
   | x == 0x8003 = ConstantAlpha
   | x == 0x8004 = OneMinusConstantAlpha
   | x == 0x308 = SrcAlphaSaturate
   | otherwise = error ("unmarshalBlendingFactor: illegal value " ++ show x)
