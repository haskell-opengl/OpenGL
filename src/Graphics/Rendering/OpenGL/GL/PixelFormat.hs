{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.PixelFormat
-- Copyright   :  (c) Sven Panne 2002-2013
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This is a purely internal module for (un-)marshaling PixelFormat.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.PixelFormat (
   PixelFormat(..), marshalPixelFormat, unmarshalPixelFormat
) where

import Graphics.Rendering.OpenGL.Raw

--------------------------------------------------------------------------------

data PixelFormat =
     ColorIndex
   | StencilIndex
   | DepthComponent
   | DepthStencil
   | Red
   | Green
   | Blue
   | Alpha
   | RG
   | RGB
   | RGBA
   | Luminance
   | LuminanceAlpha
   | RedInteger
   | GreenInteger
   | BlueInteger
   | AlphaInteger
   | RGInteger
   | RGBInteger
   | RGBAInteger
   | BGRInteger
   | BGRAInteger
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
   deriving ( Eq, Ord, Show )

marshalPixelFormat :: PixelFormat -> GLenum
marshalPixelFormat x = case x of
   ColorIndex -> gl_COLOR_INDEX
   StencilIndex -> gl_STENCIL_INDEX
   DepthComponent -> gl_DEPTH_COMPONENT
   Red -> gl_RED
   Green -> gl_GREEN
   Blue -> gl_BLUE
   Alpha -> gl_ALPHA
   RG -> gl_RG
   RGB -> gl_RGB
   RGBA -> gl_RGBA
   Luminance -> gl_LUMINANCE
   LuminanceAlpha -> gl_LUMINANCE_ALPHA
   RedInteger -> gl_RED_INTEGER
   GreenInteger -> gl_GREEN_INTEGER
   BlueInteger -> gl_BLUE_INTEGER
   AlphaInteger -> gl_ALPHA_INTEGER
   RGInteger -> gl_RG_INTEGER
   RGBInteger -> gl_RGB_INTEGER
   RGBAInteger -> gl_RGBA_INTEGER
   BGRInteger -> gl_BGR_INTEGER
   BGRAInteger -> gl_BGRA_INTEGER
   ABGR -> gl_ABGR_EXT
   BGR -> gl_BGR
   BGRA -> gl_BGRA
   CMYK -> gl_CMYK_EXT
   CMYKA -> gl_CMYKA_EXT
   FourTwoTwo -> gl_422_EXT
   FourTwoTwoRev -> gl_422_REV_EXT
   FourTwoTwoAverage -> gl_422_AVERAGE_EXT
   FourTwoTwoRevAverage -> gl_422_REV_AVERAGE_EXT
   YCBCR422 -> gl_YCBCR_422_APPLE
   DepthStencil -> gl_DEPTH_STENCIL

unmarshalPixelFormat :: GLenum -> PixelFormat
unmarshalPixelFormat x
   | x == gl_COLOR_INDEX = ColorIndex
   | x == gl_STENCIL_INDEX = StencilIndex
   | x == gl_DEPTH_COMPONENT = DepthComponent
   | x == gl_RED = Red
   | x == gl_GREEN = Green
   | x == gl_BLUE = Blue
   | x == gl_ALPHA = Alpha
   | x == gl_RG = RG
   | x == gl_RGB = RGB
   | x == gl_RGBA = RGBA
   | x == gl_LUMINANCE = Luminance
   | x == gl_LUMINANCE_ALPHA = LuminanceAlpha
   | x == gl_RED_INTEGER = RedInteger
   | x == gl_GREEN_INTEGER = GreenInteger
   | x == gl_BLUE_INTEGER = BlueInteger
   | x == gl_ALPHA_INTEGER = AlphaInteger
   | x == gl_RG_INTEGER = RGInteger
   | x == gl_RGB_INTEGER = RGBInteger
   | x == gl_RGBA_INTEGER = RGBAInteger
   | x == gl_BGR_INTEGER = BGRInteger
   | x == gl_BGRA_INTEGER = BGRAInteger
   | x == gl_ABGR_EXT = ABGR
   | x == gl_BGR = BGR
   | x == gl_BGRA = BGRA
   | x == gl_CMYK_EXT = CMYK
   | x == gl_CMYKA_EXT = CMYKA
   | x == gl_422_EXT = FourTwoTwo
   | x == gl_422_REV_EXT = FourTwoTwoRev
   | x == gl_422_AVERAGE_EXT = FourTwoTwoAverage
   | x == gl_422_REV_AVERAGE_EXT = FourTwoTwoRevAverage
   | x == gl_YCBCR_422_APPLE = YCBCR422
   | x == gl_DEPTH_STENCIL = DepthStencil
   | otherwise = error ("unmarshalPixelFormat: illegal value " ++ show x)
