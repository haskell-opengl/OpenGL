-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.PixelFormat
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- This is a purely internal module for (un-)marshaling PixelFormat.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.PixelFormat (
   PixelFormat(..), marshalPixelFormat, unmarshalPixelFormat
) where

import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLenum )

--------------------------------------------------------------------------------

data PixelFormat =
     ColorIndex
   | StencilIndex
   | DepthComponent
   | Red
   | Green
   | Blue
   | Alpha
   | RGB
   | RGBA
   | Luminance
   | LuminanceAlpha
   | ABGR
   | BGR
   | BGRA
   | CMYK
   | CMYKA
   | FourTwoTwo
   | FourTwoTwoRev
   | FourTwoTwoAverage
   | FourTwoTwoRevAverage
   | YCBCR422
   | DepthStencil
   deriving ( Eq, Ord, Show )

marshalPixelFormat :: PixelFormat -> GLenum
marshalPixelFormat x = case x of
   ColorIndex -> 0x1900
   StencilIndex -> 0x1901
   DepthComponent -> 0x1902
   Red -> 0x1903
   Green -> 0x1904
   Blue -> 0x1905
   Alpha -> 0x1906
   RGB -> 0x1907
   RGBA -> 0x1908
   Luminance -> 0x1909
   LuminanceAlpha -> 0x190a
   ABGR -> 0x8000
   BGR -> 0x80E0
   BGRA -> 0x80E1
   CMYK -> 0x800C
   CMYKA -> 0x800D
   FourTwoTwo -> 0x80CC
   FourTwoTwoRev -> 0x80CD
   FourTwoTwoAverage -> 0x80CE
   FourTwoTwoRevAverage -> 0x80CF
   YCBCR422 -> 0x85B9
   DepthStencil -> 0x84f9

unmarshalPixelFormat :: GLenum -> PixelFormat
unmarshalPixelFormat x
   | x == 0x1900 = ColorIndex
   | x == 0x1901 = StencilIndex
   | x == 0x1902 = DepthComponent
   | x == 0x1903 = Red
   | x == 0x1904 = Green
   | x == 0x1905 = Blue
   | x == 0x1906 = Alpha
   | x == 0x1907 = RGB
   | x == 0x1908 = RGBA
   | x == 0x1909 = Luminance
   | x == 0x190a = LuminanceAlpha
   | x == 0x8000 = ABGR
   | x == 0x80E0 = BGR
   | x == 0x80E1 = BGRA
   | x == 0x800C = CMYK
   | x == 0x800D = CMYKA
   | x == 0x80CC = FourTwoTwo
   | x == 0x80CD = FourTwoTwoRev
   | x == 0x80CE = FourTwoTwoAverage
   | x == 0x80CF = FourTwoTwoRevAverage
   | x == 0x85B9 = YCBCR422
   | x == 0x84f9 = DepthStencil
   | otherwise = error ("unmarshalPixelFormat: illegal value " ++ show x)
