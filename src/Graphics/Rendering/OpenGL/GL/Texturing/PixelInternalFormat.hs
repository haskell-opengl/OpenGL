{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Texturing.PixelInternalFormat
-- Copyright   :  (c) Sven Panne 2002-2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This is a purely internal module for (un-)marshaling PixelInternalFormat.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Texturing.PixelInternalFormat (
   PixelInternalFormat(..), marshalPixelInternalFormat,
   marshalPixelInternalFormat', unmarshalPixelInternalFormat,
) where

import Graphics.GL

--------------------------------------------------------------------------------

data PixelInternalFormat =
     Alpha'
   | DepthComponent'
   | Luminance'
   | LuminanceAlpha'
   | Intensity
   | R8
   | R16
   | RG8
   | RG16
   | RGB'
   | RGBA'
   | SRGB
   | SRGBAlpha
   | SLuminance
   | SLuminanceAlpha
   | Alpha4
   | Alpha8
   | Alpha12
   | Alpha16
   | DepthComponent16
   | DepthComponent24
   | DepthComponent32
   | Luminance4
   | Luminance8
   | Luminance12
   | Luminance16
   | Luminance4Alpha4
   | Luminance6Alpha2
   | Luminance8Alpha8
   | Luminance12Alpha4
   | Luminance12Alpha12
   | Luminance16Alpha16
   | Intensity4
   | Intensity8
   | Intensity12
   | Intensity16
   | R3G3B2
   | RGB4
   | RGB5
   | RGB8
   | RGB10
   | RGB12
   | RGB16
   | RGBA2
   | RGBA4
   | RGB5A1
   | RGBA8
   | RGB10A2
   | RGBA12
   | RGBA16
   | SRGB8
   | SRGB8Alpha8
   | R16F
   | RG16F
   | RGB16F
   | RGBA16F
   | R32F
   | RG32F
   | RGB32F
   | RGBA32F
   | R8I
   | R8UI
   | R16I
   | R16UI
   | R32I
   | R32UI
   | RG8I
   | RG8UI
   | RG16I
   | RG16UI
   | RG32I
   | RG32UI
   | RGB8I
   | RGB8UI
   | RGB16I
   | RGB16UI
   | RGB32I
   | RGB32UI
   | RGBA8I
   | RGBA8UI
   | RGBA16I
   | RGBA16UI
   | RGBA32I
   | RGBA32UI
   | SLuminance8
   | SLuminance8Alpha8
   | CompressedAlpha
   | CompressedLuminance
   | CompressedLuminanceAlpha
   | CompressedIntensity
   | CompressedRed
   | CompressedRG
   | CompressedRGB
   | CompressedRGBA
   | CompressedSRGB
   | CompressedSRGBAlpha
   | CompressedSLuminance
   | CompressedSLuminanceAlpha
   | CompressedRedRGTC1
   | CompressedSignedRedRGTC1
   | CompressedRG_RGTC2
   | CompressedSignedRG_RGTC2
   | DepthComponent32f
   | Depth32fStencil8
   | RGB9E5
   | R11fG11fB10f
   | StencilIndex1
   | StencilIndex4
   | StencilIndex8
   | StencilIndex16
   | RGBS3TC
   | RGB4S3TC
   | RGBAS3TC
   | RGBA4S3TC
   | RGBADXT5S3TC
   | RGBA4DXT5S3TC
   | CompressedRGBAS3TCDXT1
   | CompressedRGBAS3TCDXT3
   | CompressedRGBAS3TCDXT5
   | CompressedRGBS3TCDXT1
   | Alpha32F
   | Intensity32F
   | Luminance32F
   | LuminanceAlpha32F
   | Alpha16F
   | Intensity16F
   | Luminance16F
   | LuminanceAlpha16F
   | Depth24Stencil8
   deriving ( Eq, Ord, Show )

marshalPixelInternalFormat :: PixelInternalFormat -> GLint
marshalPixelInternalFormat x = fromIntegral $ case x of
   Alpha' -> GL_ALPHA
   DepthComponent' -> GL_DEPTH_COMPONENT
   Luminance' -> GL_LUMINANCE
   LuminanceAlpha' -> GL_LUMINANCE_ALPHA
   R8 -> GL_R8
   R16 -> GL_R16
   RG8 -> GL_RG8
   RG16 -> GL_RG16
   RGB' -> GL_RGB
   RGBA' -> GL_RGBA
   SRGB -> GL_SRGB
   SRGBAlpha -> GL_SRGB_ALPHA
   SLuminance -> GL_SLUMINANCE
   SLuminanceAlpha -> GL_SLUMINANCE_ALPHA
   Alpha4 -> GL_ALPHA4
   Alpha8 -> GL_ALPHA8
   Alpha12 -> GL_ALPHA12
   Alpha16 -> GL_ALPHA16
   DepthComponent16 -> GL_DEPTH_COMPONENT16
   DepthComponent24 -> GL_DEPTH_COMPONENT24
   DepthComponent32 -> GL_DEPTH_COMPONENT32
   Luminance4 -> GL_LUMINANCE4
   Luminance8 -> GL_LUMINANCE8
   Luminance12 -> GL_LUMINANCE12
   Luminance16 -> GL_LUMINANCE16
   Luminance4Alpha4 -> GL_LUMINANCE4_ALPHA4
   Luminance6Alpha2 -> GL_LUMINANCE6_ALPHA2
   Luminance8Alpha8 -> GL_LUMINANCE8_ALPHA8
   Luminance12Alpha4 -> GL_LUMINANCE12_ALPHA4
   Luminance12Alpha12 -> GL_LUMINANCE12_ALPHA12
   Luminance16Alpha16 -> GL_LUMINANCE16_ALPHA16
   Intensity -> GL_INTENSITY
   Intensity4 -> GL_INTENSITY4
   Intensity8 -> GL_INTENSITY8
   Intensity12 -> GL_INTENSITY12
   Intensity16 -> GL_INTENSITY16
   R3G3B2 -> GL_R3_G3_B2
   RGB4 -> GL_RGB4
   RGB5 -> GL_RGB5
   RGB8 -> GL_RGB8
   RGB10 -> GL_RGB10
   RGB12 -> GL_RGB12
   RGB16 -> GL_RGB16
   RGBA2 -> GL_RGBA2
   RGBA4 -> GL_RGBA4
   RGB5A1 -> GL_RGB5_A1
   RGBA8 -> GL_RGBA8
   RGB10A2 -> GL_RGB10_A2
   RGBA12 -> GL_RGBA12
   RGBA16 -> GL_RGBA16
   SRGB8 -> GL_SRGB8
   SRGB8Alpha8 -> GL_SRGB8_ALPHA8
   R16F -> GL_R16F
   RG16F -> GL_RG16F
   RGB16F -> GL_RGB16F
   RGBA16F -> GL_RGBA16F
   R32F -> GL_R32F
   RG32F -> GL_RG32F
   RGB32F -> GL_RGB32F
   RGBA32F -> GL_RGBA32F
   R8I -> GL_R8I
   R8UI -> GL_R8UI
   R16I -> GL_R16I
   R16UI -> GL_R16UI
   R32I -> GL_R32I
   R32UI -> GL_R32UI
   RG8I -> GL_RG8I
   RG8UI -> GL_RG8UI
   RG16I -> GL_RG16I
   RG16UI -> GL_RG16UI
   RG32I -> GL_R32I
   RG32UI -> GL_R32UI
   RGB8I -> GL_RGB8I
   RGB8UI -> GL_RGB8UI
   RGB16I -> GL_RGB16I
   RGB16UI -> GL_RGB16UI
   RGB32I -> GL_RGB32I
   RGB32UI -> GL_RGB32UI
   RGBA8I -> GL_RGBA8I
   RGBA8UI -> GL_RGBA8UI
   RGBA16I -> GL_RGBA16I
   RGBA16UI -> GL_RGBA16UI
   RGBA32I -> GL_RGBA32I
   RGBA32UI -> GL_RGBA32UI
   SLuminance8 -> GL_SLUMINANCE8
   SLuminance8Alpha8 -> GL_SLUMINANCE8_ALPHA8
   CompressedAlpha -> GL_COMPRESSED_ALPHA
   CompressedLuminance -> GL_COMPRESSED_LUMINANCE
   CompressedLuminanceAlpha -> GL_COMPRESSED_LUMINANCE_ALPHA
   CompressedIntensity -> GL_COMPRESSED_INTENSITY
   CompressedRed -> GL_COMPRESSED_RED
   CompressedRG -> GL_COMPRESSED_RG
   CompressedRGB -> GL_COMPRESSED_RGB
   CompressedRGBA -> GL_COMPRESSED_RGBA
   CompressedSRGB -> GL_COMPRESSED_SRGB
   CompressedSRGBAlpha -> GL_COMPRESSED_SRGB_ALPHA
   CompressedSLuminance -> GL_COMPRESSED_SLUMINANCE
   CompressedSLuminanceAlpha -> GL_COMPRESSED_SLUMINANCE_ALPHA
   CompressedRedRGTC1 -> GL_COMPRESSED_RED_RGTC1
   CompressedSignedRedRGTC1 -> GL_COMPRESSED_SIGNED_RED_RGTC1
   CompressedRG_RGTC2 -> GL_COMPRESSED_RG_RGTC2
   CompressedSignedRG_RGTC2 -> GL_COMPRESSED_SIGNED_RG_RGTC2
   DepthComponent32f -> GL_DEPTH_COMPONENT32F
   Depth32fStencil8 -> GL_DEPTH32F_STENCIL8
   RGB9E5 -> GL_RGB9_E5
   R11fG11fB10f -> GL_R11F_G11F_B10F
   StencilIndex1 -> GL_STENCIL_INDEX1
   StencilIndex4 -> GL_STENCIL_INDEX4
   StencilIndex8 -> GL_STENCIL_INDEX8
   StencilIndex16 -> GL_STENCIL_INDEX16
   RGBS3TC -> GL_RGB_S3TC
   RGB4S3TC -> GL_RGB4_S3TC
   RGBAS3TC -> GL_RGBA_S3TC
   RGBA4S3TC -> GL_RGBA4_S3TC
   RGBADXT5S3TC -> GL_RGBA_DXT5_S3TC
   RGBA4DXT5S3TC -> GL_RGBA4_DXT5_S3TC
   CompressedRGBAS3TCDXT1 -> GL_COMPRESSED_RGBA_S3TC_DXT1_EXT
   CompressedRGBAS3TCDXT3 -> GL_COMPRESSED_RGBA_S3TC_DXT3_EXT
   CompressedRGBAS3TCDXT5 -> GL_COMPRESSED_RGBA_S3TC_DXT5_EXT
   CompressedRGBS3TCDXT1 -> GL_COMPRESSED_RGB_S3TC_DXT1_EXT
   Alpha32F -> GL_ALPHA32F_ARB
   Intensity32F -> GL_INTENSITY32F_ARB
   Luminance32F -> GL_LUMINANCE32F_ARB
   LuminanceAlpha32F -> GL_LUMINANCE_ALPHA32F_ARB
   Alpha16F -> GL_ALPHA16F_ARB
   Intensity16F -> GL_INTENSITY16F_ARB
   Luminance16F -> GL_LUMINANCE16F_ARB
   LuminanceAlpha16F -> GL_LUMINANCE_ALPHA16F_ARB
   Depth24Stencil8 -> GL_DEPTH24_STENCIL8_EXT

-- *sigh* The OpenGL API is sometimes a bit creative in its usage of types...
marshalPixelInternalFormat' :: PixelInternalFormat -> GLenum
marshalPixelInternalFormat' = fromIntegral . marshalPixelInternalFormat

unmarshalPixelInternalFormat :: GLint -> PixelInternalFormat
unmarshalPixelInternalFormat x
   | y == GL_ALPHA = Alpha'
   | y == GL_DEPTH_COMPONENT = DepthComponent'
   | y == GL_LUMINANCE = Luminance'
   | y == GL_LUMINANCE_ALPHA = LuminanceAlpha'
   | y == GL_RGB = RGB'
   | y == GL_RGBA = RGBA'
   | y == GL_SRGB = SRGB
   | y == GL_SRGB_ALPHA = SRGBAlpha
   | y == GL_SLUMINANCE = SLuminance
   | y == GL_SLUMINANCE_ALPHA = SLuminanceAlpha
   | y == GL_ALPHA4 = Alpha4
   | y == GL_ALPHA8 = Alpha8
   | y == GL_ALPHA12 = Alpha12
   | y == GL_ALPHA16 = Alpha16
   | y == GL_DEPTH_COMPONENT16 = DepthComponent16
   | y == GL_DEPTH_COMPONENT24 = DepthComponent24
   | y == GL_DEPTH_COMPONENT32 = DepthComponent32
   | y == GL_LUMINANCE4 = Luminance4
   | y == GL_LUMINANCE8 = Luminance8
   | y == GL_LUMINANCE12 = Luminance12
   | y == GL_LUMINANCE16 = Luminance16
   | y == GL_LUMINANCE4_ALPHA4 = Luminance4Alpha4
   | y == GL_LUMINANCE6_ALPHA2 = Luminance6Alpha2
   | y == GL_LUMINANCE8_ALPHA8 = Luminance8Alpha8
   | y == GL_LUMINANCE12_ALPHA4 = Luminance12Alpha4
   | y == GL_LUMINANCE12_ALPHA12 = Luminance12Alpha12
   | y == GL_LUMINANCE16_ALPHA16 = Luminance16Alpha16
   | y == GL_INTENSITY = Intensity
   | y == GL_INTENSITY4 = Intensity4
   | y == GL_INTENSITY8 = Intensity8
   | y == GL_INTENSITY12 = Intensity12
   | y == GL_INTENSITY16 = Intensity16
   | y == GL_R3_G3_B2 = R3G3B2
   | y == GL_RGB4 = RGB4
   | y == GL_RGB5 = RGB5
   | y == GL_RGB8 = RGB8
   | y == GL_RGB10 = RGB10
   | y == GL_RGB12 = RGB12
   | y == GL_RGB16 = RGB16
   | y == GL_RGBA2 = RGBA2
   | y == GL_RGBA4 = RGBA4
   | y == GL_RGB5_A1 = RGB5A1
   | y == GL_RGBA8 = RGBA8
   | y == GL_RGB10_A2 = RGB10A2
   | y == GL_RGBA12 = RGBA12
   | y == GL_RGBA16 = RGBA16
   | y == GL_SRGB8 = SRGB8
   | y == GL_SRGB8_ALPHA8 = SRGB8Alpha8
   | y == GL_R16F = R16F
   | y == GL_RG16F = RG16F
   | y == GL_RGB16F = RGB16F
   | y == GL_RGBA16F = RGBA16F
   | y == GL_R32F = R32F
   | y == GL_RG32F = RG32F
   | y == GL_RGB32F = RGB32F
   | y == GL_RGBA32F = RGBA32F
   | y == GL_R8I = R8I
   | y == GL_R8UI = R8UI
   | y == GL_R16I = R16I
   | y == GL_R16UI = R16UI
   | y == GL_R32I = R32I
   | y == GL_R32UI = R32UI
   | y == GL_RG8I = RG8I
   | y == GL_RG8UI = RG8UI
   | y == GL_RG16I = RG16I
   | y == GL_RG16UI = RG16UI
   | y == GL_R32I = RG32I
   | y == GL_R32UI = RG32UI
   | y == GL_RGB8I = RGB8I
   | y == GL_RGB8UI = RGB8UI
   | y == GL_RGB16I = RGB16I
   | y == GL_RGB16UI = RGB16UI
   | y == GL_RGB32I = RGB32I
   | y == GL_RGB32UI = RGB32UI
   | y == GL_RGBA8I = RGBA8I
   | y == GL_RGBA8UI = RGBA8UI
   | y == GL_RGBA16I = RGBA16I
   | y == GL_RGBA16UI = RGBA16UI
   | y == GL_RGBA32I = RGBA32I
   | y == GL_RGBA32UI = RGBA32UI
   | y == GL_SLUMINANCE8 = SLuminance8
   | y == GL_SLUMINANCE8_ALPHA8 = SLuminance8Alpha8
   | y == GL_COMPRESSED_ALPHA = CompressedAlpha
   | y == GL_COMPRESSED_LUMINANCE = CompressedLuminance
   | y == GL_COMPRESSED_LUMINANCE_ALPHA = CompressedLuminanceAlpha
   | y == GL_COMPRESSED_INTENSITY = CompressedIntensity
   | y == GL_COMPRESSED_RED = CompressedRed
   | y == GL_COMPRESSED_RG = CompressedRG
   | y == GL_COMPRESSED_RGB = CompressedRGB
   | y == GL_COMPRESSED_RGBA = CompressedRGBA
   | y == GL_COMPRESSED_SRGB = CompressedSRGB
   | y == GL_COMPRESSED_SRGB_ALPHA = CompressedSRGBAlpha
   | y == GL_COMPRESSED_SLUMINANCE = CompressedSLuminance
   | y == GL_COMPRESSED_SLUMINANCE_ALPHA = CompressedSLuminanceAlpha
   | y == GL_COMPRESSED_RED_RGTC1 = CompressedRedRGTC1
   | y == GL_COMPRESSED_SIGNED_RED_RGTC1 = CompressedSignedRedRGTC1
   | y == GL_COMPRESSED_RG_RGTC2 = CompressedRG_RGTC2
   | y == GL_COMPRESSED_SIGNED_RG_RGTC2 = CompressedSignedRG_RGTC2
   | y == GL_DEPTH_COMPONENT32F = DepthComponent32f
   | y == GL_DEPTH32F_STENCIL8 = Depth32fStencil8
   | y == GL_RGB9_E5 = RGB9E5
   | y == GL_STENCIL_INDEX1 = StencilIndex1
   | y == GL_STENCIL_INDEX4 = StencilIndex4
   | y == GL_STENCIL_INDEX8 = StencilIndex8
   | y == GL_STENCIL_INDEX16 = StencilIndex16
   | y == GL_RGB_S3TC = RGBS3TC
   | y == GL_RGB4_S3TC = RGB4S3TC
   | y == GL_RGBA_S3TC = RGBAS3TC
   | y == GL_RGBA4_S3TC = RGBA4S3TC
   | y == GL_RGBA_DXT5_S3TC = RGBADXT5S3TC
   | y == GL_RGBA4_DXT5_S3TC = RGBA4DXT5S3TC
   | y == GL_COMPRESSED_RGBA_S3TC_DXT1_EXT = CompressedRGBAS3TCDXT1
   | y == GL_COMPRESSED_RGBA_S3TC_DXT3_EXT = CompressedRGBAS3TCDXT3
   | y == GL_COMPRESSED_RGBA_S3TC_DXT5_EXT = CompressedRGBAS3TCDXT5
   | y == GL_COMPRESSED_RGB_S3TC_DXT1_EXT = CompressedRGBS3TCDXT1
   | y == GL_ALPHA32F_ARB = Alpha32F
   | y == GL_INTENSITY32F_ARB = Intensity32F
   | y == GL_LUMINANCE32F_ARB = Luminance32F
   | y == GL_LUMINANCE_ALPHA32F_ARB = LuminanceAlpha32F
   | y == GL_ALPHA16F_ARB = Alpha16F
   | y == GL_INTENSITY16F_ARB = Intensity16F
   | y == GL_LUMINANCE16F_ARB = Luminance16F
   | y == GL_LUMINANCE_ALPHA16F_ARB = LuminanceAlpha16F
   | y == GL_DEPTH24_STENCIL8_EXT = Depth24Stencil8
   -- legacy values
   | y == 1 = Luminance'
   | y == 2 = LuminanceAlpha'
   | y == 3 = RGB'
   | y == 4 = RGBA'
   | otherwise = error ("unmarshalPixelInternalFormat: illegal value " ++ show x)
   where y = fromIntegral x

-- Note: The following formats are still missing, it's a bit unclear how to
-- handle these nicely. From the EXT_texture_sRGB spec:
--
--    Accepted by the <internalformat> parameter of TexImage2D, CopyTexImage2D,
--    and CompressedTexImage2DARB and the <format> parameter of
--    CompressedTexSubImage2DARB:
--
--       COMPRESSED_SRGB_S3TC_DXT1_EXT                  0x8C4C
--       COMPRESSED_SRGB_ALPHA_S3TC_DXT1_EXT            0x8C4D
--       COMPRESSED_SRGB_ALPHA_S3TC_DXT3_EXT            0x8C4E
--       COMPRESSED_SRGB_ALPHA_S3TC_DXT5_EXT            0x8C4F
