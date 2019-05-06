{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.PixelFormat
-- Copyright   :  (c) Sven Panne 2002-2019
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

import Graphics.GL

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
   ColorIndex -> GL_COLOR_INDEX
   StencilIndex -> GL_STENCIL_INDEX
   DepthComponent -> GL_DEPTH_COMPONENT
   Red -> GL_RED
   Green -> GL_GREEN
   Blue -> GL_BLUE
   Alpha -> GL_ALPHA
   RG -> GL_RG
   RGB -> GL_RGB
   RGBA -> GL_RGBA
   Luminance -> GL_LUMINANCE
   LuminanceAlpha -> GL_LUMINANCE_ALPHA
   RedInteger -> GL_RED_INTEGER
   GreenInteger -> GL_GREEN_INTEGER
   BlueInteger -> GL_BLUE_INTEGER
   AlphaInteger -> GL_ALPHA_INTEGER
   RGInteger -> GL_RG_INTEGER
   RGBInteger -> GL_RGB_INTEGER
   RGBAInteger -> GL_RGBA_INTEGER
   BGRInteger -> GL_BGR_INTEGER
   BGRAInteger -> GL_BGRA_INTEGER
   ABGR -> GL_ABGR_EXT
   BGR -> GL_BGR
   BGRA -> GL_BGRA
   CMYK -> GL_CMYK_EXT
   CMYKA -> GL_CMYKA_EXT
   FourTwoTwo -> GL_422_EXT
   FourTwoTwoRev -> GL_422_REV_EXT
   FourTwoTwoAverage -> GL_422_AVERAGE_EXT
   FourTwoTwoRevAverage -> GL_422_REV_AVERAGE_EXT
   YCBCR422 -> GL_YCBCR_422_APPLE
   DepthStencil -> GL_DEPTH_STENCIL

unmarshalPixelFormat :: GLenum -> PixelFormat
unmarshalPixelFormat x
   | x == GL_COLOR_INDEX = ColorIndex
   | x == GL_STENCIL_INDEX = StencilIndex
   | x == GL_DEPTH_COMPONENT = DepthComponent
   | x == GL_RED = Red
   | x == GL_GREEN = Green
   | x == GL_BLUE = Blue
   | x == GL_ALPHA = Alpha
   | x == GL_RG = RG
   | x == GL_RGB = RGB
   | x == GL_RGBA = RGBA
   | x == GL_LUMINANCE = Luminance
   | x == GL_LUMINANCE_ALPHA = LuminanceAlpha
   | x == GL_RED_INTEGER = RedInteger
   | x == GL_GREEN_INTEGER = GreenInteger
   | x == GL_BLUE_INTEGER = BlueInteger
   | x == GL_ALPHA_INTEGER = AlphaInteger
   | x == GL_RG_INTEGER = RGInteger
   | x == GL_RGB_INTEGER = RGBInteger
   | x == GL_RGBA_INTEGER = RGBAInteger
   | x == GL_BGR_INTEGER = BGRInteger
   | x == GL_BGRA_INTEGER = BGRAInteger
   | x == GL_ABGR_EXT = ABGR
   | x == GL_BGR = BGR
   | x == GL_BGRA = BGRA
   | x == GL_CMYK_EXT = CMYK
   | x == GL_CMYKA_EXT = CMYKA
   | x == GL_422_EXT = FourTwoTwo
   | x == GL_422_REV_EXT = FourTwoTwoRev
   | x == GL_422_AVERAGE_EXT = FourTwoTwoAverage
   | x == GL_422_REV_AVERAGE_EXT = FourTwoTwoRevAverage
   | x == GL_YCBCR_422_APPLE = YCBCR422
   | x == GL_DEPTH_STENCIL = DepthStencil
   | otherwise = error ("unmarshalPixelFormat: illegal value " ++ show x)
