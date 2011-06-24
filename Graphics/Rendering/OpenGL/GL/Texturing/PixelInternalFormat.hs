-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Texturing.PixelInternalFormat
-- Copyright   :  (c) Sven Panne 2002-2009
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
--
-- Maintainer  :  sven.panne@aedion.de
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

import Graphics.Rendering.OpenGL.Raw.ARB.Compatibility (
   gl_ALPHA12, gl_ALPHA16, gl_ALPHA4, gl_ALPHA8, gl_COMPRESSED_ALPHA,
   gl_COMPRESSED_INTENSITY, gl_COMPRESSED_LUMINANCE,
   gl_COMPRESSED_LUMINANCE_ALPHA, gl_COMPRESSED_SLUMINANCE,
   gl_COMPRESSED_SLUMINANCE_ALPHA, gl_INTENSITY, gl_INTENSITY12, gl_INTENSITY16,
   gl_INTENSITY4, gl_INTENSITY8, gl_LUMINANCE, gl_LUMINANCE12,
   gl_LUMINANCE12_ALPHA12, gl_LUMINANCE12_ALPHA4, gl_LUMINANCE16,
   gl_LUMINANCE16_ALPHA16, gl_LUMINANCE4, gl_LUMINANCE4_ALPHA4,
   gl_LUMINANCE6_ALPHA2, gl_LUMINANCE8, gl_LUMINANCE8_ALPHA8,
   gl_LUMINANCE_ALPHA, gl_SLUMINANCE, gl_SLUMINANCE8, gl_SLUMINANCE8_ALPHA8,
   gl_SLUMINANCE_ALPHA )
import Graphics.Rendering.OpenGL.Raw.Core31

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
   deriving ( Eq, Ord, Show )

marshalPixelInternalFormat :: PixelInternalFormat -> GLint
marshalPixelInternalFormat x = fromIntegral $ case x of
   Alpha' -> gl_ALPHA
   DepthComponent' -> gl_DEPTH_COMPONENT
   Luminance' -> gl_LUMINANCE
   LuminanceAlpha' -> gl_LUMINANCE_ALPHA
   R8 -> gl_R8
   R16 -> gl_R16
   RG8 -> gl_RG8
   RG16 -> gl_RG16
   RGB' -> gl_RGB
   RGBA' -> gl_RGBA
   SRGB -> gl_SRGB
   SRGBAlpha -> gl_SRGB_ALPHA
   SLuminance -> gl_SLUMINANCE
   SLuminanceAlpha -> gl_SLUMINANCE_ALPHA
   Alpha4 -> gl_ALPHA4
   Alpha8 -> gl_ALPHA8
   Alpha12 -> gl_ALPHA12
   Alpha16 -> gl_ALPHA16
   DepthComponent16 -> gl_DEPTH_COMPONENT16
   DepthComponent24 -> gl_DEPTH_COMPONENT24
   DepthComponent32 -> gl_DEPTH_COMPONENT32
   Luminance4 -> gl_LUMINANCE4
   Luminance8 -> gl_LUMINANCE8
   Luminance12 -> gl_LUMINANCE12
   Luminance16 -> gl_LUMINANCE16
   Luminance4Alpha4 -> gl_LUMINANCE4_ALPHA4
   Luminance6Alpha2 -> gl_LUMINANCE6_ALPHA2
   Luminance8Alpha8 -> gl_LUMINANCE8_ALPHA8
   Luminance12Alpha4 -> gl_LUMINANCE12_ALPHA4
   Luminance12Alpha12 -> gl_LUMINANCE12_ALPHA12
   Luminance16Alpha16 -> gl_LUMINANCE16_ALPHA16
   Intensity -> gl_INTENSITY
   Intensity4 -> gl_INTENSITY4
   Intensity8 -> gl_INTENSITY8
   Intensity12 -> gl_INTENSITY12
   Intensity16 -> gl_INTENSITY16
   R3G3B2 -> gl_R3_G3_B2
   RGB4 -> gl_RGB4
   RGB5 -> gl_RGB5
   RGB8 -> gl_RGB8
   RGB10 -> gl_RGB10
   RGB12 -> gl_RGB12
   RGB16 -> gl_RGB16
   RGBA2 -> gl_RGBA2
   RGBA4 -> gl_RGBA4
   RGB5A1 -> gl_RGB5_A1
   RGBA8 -> gl_RGBA8
   RGB10A2 -> gl_RGB10_A2
   RGBA12 -> gl_RGBA12
   RGBA16 -> gl_RGBA16
   SRGB8 -> gl_SRGB8
   SRGB8Alpha8 -> gl_SRGB8_ALPHA8
   R16F -> gl_R16F
   RG16F -> gl_RG16F
   RGB16F -> gl_RGB16F
   RGBA16F -> gl_RGBA16F
   R32F -> gl_R32F
   RG32F -> gl_RG32F
   RGB32F -> gl_RGB32F
   RGBA32F -> gl_RGBA32F
   R8I -> gl_R8I
   R8UI -> gl_R8UI
   R16I -> gl_R16I
   R16UI -> gl_R16UI
   R32I -> gl_R32I
   R32UI -> gl_R32UI
   RG8I -> gl_RG8I
   RG8UI -> gl_RG8UI
   RG16I -> gl_RG16I
   RG16UI -> gl_RG16UI
   RG32I -> gl_R32I
   RG32UI -> gl_R32UI
   RGB8I -> gl_RGB8I
   RGB8UI -> gl_RGB8UI
   RGB16I -> gl_RGB16I
   RGB16UI -> gl_RGB16UI
   RGB32I -> gl_RGB32I
   RGB32UI -> gl_RGB32UI
   RGBA8I -> gl_RGBA8I
   RGBA8UI -> gl_RGBA8UI
   RGBA16I -> gl_RGBA16I
   RGBA16UI -> gl_RGBA16UI
   RGBA32I -> gl_RGBA32I
   RGBA32UI -> gl_RGBA32UI
   SLuminance8 -> gl_SLUMINANCE8
   SLuminance8Alpha8 -> gl_SLUMINANCE8_ALPHA8
   CompressedAlpha -> gl_COMPRESSED_ALPHA
   CompressedLuminance -> gl_COMPRESSED_LUMINANCE
   CompressedLuminanceAlpha -> gl_COMPRESSED_LUMINANCE_ALPHA
   CompressedIntensity -> gl_COMPRESSED_INTENSITY
   CompressedRed -> gl_COMPRESSED_RED
   CompressedRG -> gl_COMPRESSED_RG
   CompressedRGB -> gl_COMPRESSED_RGB
   CompressedRGBA -> gl_COMPRESSED_RGBA
   CompressedSRGB -> gl_COMPRESSED_SRGB
   CompressedSRGBAlpha -> gl_COMPRESSED_SRGB_ALPHA
   CompressedSLuminance -> gl_COMPRESSED_SLUMINANCE
   CompressedSLuminanceAlpha -> gl_COMPRESSED_SLUMINANCE_ALPHA
   CompressedRedRGTC1 -> gl_COMPRESSED_RED_RGTC1
   CompressedSignedRedRGTC1 -> gl_COMPRESSED_SIGNED_RED_RGTC1
   CompressedRG_RGTC2 -> gl_COMPRESSED_RG_RGTC2
   CompressedSignedRG_RGTC2 -> gl_COMPRESSED_SIGNED_RG_RGTC2
   DepthComponent32f -> gl_DEPTH_COMPONENT32F
   Depth32fStencil8 -> gl_DEPTH32F_STENCIL8
   RGB9E5 -> gl_RGB9_E5
   R11fG11fB10f -> gl_R11F_G11F_B10F
   StencilIndex1 -> gl_STENCIL_INDEX1
   StencilIndex4 -> gl_STENCIL_INDEX4
   StencilIndex8 -> gl_STENCIL_INDEX8
   StencilIndex16 -> gl_STENCIL_INDEX16

-- *sigh* The OpenGL API is sometimes a bit creative in its usage of types...
marshalPixelInternalFormat' :: PixelInternalFormat -> GLenum
marshalPixelInternalFormat' = fromIntegral . marshalPixelInternalFormat

unmarshalPixelInternalFormat :: GLint -> PixelInternalFormat
unmarshalPixelInternalFormat x
   | y == gl_ALPHA = Alpha'
   | y == gl_DEPTH_COMPONENT = DepthComponent'
   | y == gl_LUMINANCE = Luminance'
   | y == gl_LUMINANCE_ALPHA = LuminanceAlpha'
   | y == gl_RGB = RGB'
   | y == gl_RGBA = RGBA'
   | y == gl_SRGB = SRGB
   | y == gl_SRGB_ALPHA = SRGBAlpha
   | y == gl_SLUMINANCE = SLuminance
   | y == gl_SLUMINANCE_ALPHA = SLuminanceAlpha
   | y == gl_ALPHA4 = Alpha4
   | y == gl_ALPHA8 = Alpha8
   | y == gl_ALPHA12 = Alpha12
   | y == gl_ALPHA16 = Alpha16
   | y == gl_DEPTH_COMPONENT16 = DepthComponent16
   | y == gl_DEPTH_COMPONENT24 = DepthComponent24
   | y == gl_DEPTH_COMPONENT32 = DepthComponent32
   | y == gl_LUMINANCE4 = Luminance4
   | y == gl_LUMINANCE8 = Luminance8
   | y == gl_LUMINANCE12 = Luminance12
   | y == gl_LUMINANCE16 = Luminance16
   | y == gl_LUMINANCE4_ALPHA4 = Luminance4Alpha4
   | y == gl_LUMINANCE6_ALPHA2 = Luminance6Alpha2
   | y == gl_LUMINANCE8_ALPHA8 = Luminance8Alpha8
   | y == gl_LUMINANCE12_ALPHA4 = Luminance12Alpha4
   | y == gl_LUMINANCE12_ALPHA12 = Luminance12Alpha12
   | y == gl_LUMINANCE16_ALPHA16 = Luminance16Alpha16
   | y == gl_INTENSITY = Intensity
   | y == gl_INTENSITY4 = Intensity4
   | y == gl_INTENSITY8 = Intensity8
   | y == gl_INTENSITY12 = Intensity12
   | y == gl_INTENSITY16 = Intensity16
   | y == gl_R3_G3_B2 = R3G3B2
   | y == gl_RGB4 = RGB4
   | y == gl_RGB5 = RGB5
   | y == gl_RGB8 = RGB8
   | y == gl_RGB10 = RGB10
   | y == gl_RGB12 = RGB12
   | y == gl_RGB16 = RGB16
   | y == gl_RGBA2 = RGBA2
   | y == gl_RGBA4 = RGBA4
   | y == gl_RGB5_A1 = RGB5A1
   | y == gl_RGBA8 = RGBA8
   | y == gl_RGB10_A2 = RGB10A2
   | y == gl_RGBA12 = RGBA12
   | y == gl_RGBA16 = RGBA16
   | y == gl_SRGB8 = SRGB8
   | y == gl_SRGB8_ALPHA8 = SRGB8Alpha8
   | y == gl_R16F = R16F
   | y == gl_RG16F = RG16F
   | y == gl_RGB16F = RGB16F
   | y == gl_RGBA16F = RGBA16F
   | y == gl_R32F = R32F
   | y == gl_RG32F = RG32F
   | y == gl_RGB32F = RGB32F
   | y == gl_RGBA32F = RGBA32F
   | y == gl_R8I = R8I
   | y == gl_R8UI = R8UI
   | y == gl_R16I = R16I
   | y == gl_R16UI = R16UI
   | y == gl_R32I = R32I
   | y == gl_R32UI = R32UI
   | y == gl_RG8I = RG8I
   | y == gl_RG8UI = RG8UI
   | y == gl_RG16I = RG16I
   | y == gl_RG16UI = RG16UI
   | y == gl_R32I = RG32I
   | y == gl_R32UI = RG32UI
   | y == gl_RGB8I = RGB8I
   | y == gl_RGB8UI = RGB8UI
   | y == gl_RGB16I = RGB16I
   | y == gl_RGB16UI = RGB16UI
   | y == gl_RGB32I = RGB32I
   | y == gl_RGB32UI = RGB32UI
   | y == gl_RGBA8I = RGBA8I
   | y == gl_RGBA8UI = RGBA8UI
   | y == gl_RGBA16I = RGBA16I
   | y == gl_RGBA16UI = RGBA16UI
   | y == gl_RGBA32I = RGBA32I
   | y == gl_RGBA32UI = RGBA32UI
   | y == gl_SLUMINANCE8 = SLuminance8
   | y == gl_SLUMINANCE8_ALPHA8 = SLuminance8Alpha8
   | y == gl_COMPRESSED_ALPHA = CompressedAlpha
   | y == gl_COMPRESSED_LUMINANCE = CompressedLuminance
   | y == gl_COMPRESSED_LUMINANCE_ALPHA = CompressedLuminanceAlpha
   | y == gl_COMPRESSED_INTENSITY = CompressedIntensity
   | y == gl_COMPRESSED_RED = CompressedRed
   | y == gl_COMPRESSED_RG = CompressedRG
   | y == gl_COMPRESSED_RGB = CompressedRGB
   | y == gl_COMPRESSED_RGBA = CompressedRGBA
   | y == gl_COMPRESSED_SRGB = CompressedSRGB
   | y == gl_COMPRESSED_SRGB_ALPHA = CompressedSRGBAlpha
   | y == gl_COMPRESSED_SLUMINANCE = CompressedSLuminance
   | y == gl_COMPRESSED_SLUMINANCE_ALPHA = CompressedSLuminanceAlpha
   | y == gl_COMPRESSED_RED_RGTC1 = CompressedRedRGTC1
   | y == gl_COMPRESSED_SIGNED_RED_RGTC1 = CompressedSignedRedRGTC1
   | y == gl_COMPRESSED_RG_RGTC2 = CompressedRG_RGTC2
   | y == gl_COMPRESSED_SIGNED_RG_RGTC2 = CompressedSignedRG_RGTC2
   | y == gl_DEPTH_COMPONENT32F = DepthComponent32f
   | y == gl_DEPTH32F_STENCIL8 = Depth32fStencil8
   | y == gl_RGB9_E5 = RGB9E5
   | y == gl_STENCIL_INDEX1 = StencilIndex1
   | y == gl_STENCIL_INDEX4 = StencilIndex4
   | y == gl_STENCIL_INDEX8 = StencilIndex8
   | y == gl_STENCIL_INDEX16 = StencilIndex16
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
