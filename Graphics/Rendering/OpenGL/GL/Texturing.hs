--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Texturing
-- Copyright   :  (c) Sven Panne 2002-2004
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
-- This module corresponds to section 3.8 (Texturing) of the OpenGL 1.5 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Texturing (
   -- * Texture-related Data Types
   TextureTarget(..), Level, PixelInternalFormat(..), Border,
   TexturePosition1D(..), TexturePosition2D(..), TexturePosition3D(..),
   TextureSize1D(..), TextureSize2D(..), TextureSize3D(..),

   -- * Texture Image Specification
   texImage1D, texImage2D, texImage3D,

   -- * Alternate Texture Image Specification Commands
   copyTexImage1D, copyTexImage2D,
   texSubImage1D, texSubImage2D, texSubImage3D,
   copyTexSubImage1D, copyTexSubImage2D, copyTexSubImage3D,

   -- * Compressed Texture Images
   CompressedTextureFormat(..), compressedTextureFormats,
   CompressedPixelData(..),
   compressedTexImage1D, compressedTexImage2D, compressedTexImage3D,
   compressedTexSubImage1D, compressedTexSubImage2D, compressedTexSubImage3D,

   -- * Texture Parameters
   TextureFilter(..), textureFilter,
   Repetition(..), Clamping(..), textureWrapMode,
   textureBorderColor, textureLODRange, textureLevelRange,
   textureMaxAnisotropy, maxTextureMaxAnisotropy,

   -- * Texture Objects
   TextureObject, defaultTextureObject, textureBinding,
   textureResident, areTexturesResident,
   texturePriority, prioritizeTextures,

   -- * Texture Environment and Texture Functions
   TextureEnvMode(..), textureEnvMode,
   texture,

   -- * Texture Queries
   getTexImage, getCompressedTexImage,
   textureInternalFormat, textureSize1D, textureSize2D, textureSize3D,
   textureBorder, textureRGBASizes, textureIntensitySize, textureLuminanceSize,
   textureIndexSize, textureDepthBits, textureCompressedImageSize,
   textureProxyOK
) where

import Graphics.Rendering.OpenGL.GL.Capability (
   EnableCap(CapTexture1D,CapTexture2D,CapTexture3D,CapTextureCubeMap),
   Capability, makeCapability )
import Graphics.Rendering.OpenGL.GL.PixelInternalFormat (
   PixelInternalFormat(..) )
import Graphics.Rendering.OpenGL.GL.StateVar ( StateVar )
import Graphics.Rendering.OpenGL.GL.Texturing.Environments (
   TextureEnvMode(..), textureEnvMode )
import Graphics.Rendering.OpenGL.GL.Texturing.TextureTarget (
   TextureTarget(..) )
import Graphics.Rendering.OpenGL.GL.Texturing.LevelParameters (
   textureInternalFormat, textureSize1D, textureSize2D, textureSize3D,
   textureBorder, textureRGBASizes, textureIntensitySize, textureLuminanceSize,
   textureIndexSize, textureDepthBits, textureCompressedImageSize,
   textureProxyOK )
import Graphics.Rendering.OpenGL.GL.Texturing.Objects (
   TextureObject, defaultTextureObject, textureBinding, areTexturesResident,
   prioritizeTextures )
import Graphics.Rendering.OpenGL.GL.Texturing.Parameters (
   TextureFilter(..), textureFilter, Repetition(..), Clamping(..),
   textureWrapMode, textureBorderColor, textureLODRange, textureLevelRange,
   textureMaxAnisotropy, maxTextureMaxAnisotropy, textureResident,
   texturePriority )
import Graphics.Rendering.OpenGL.GL.Texturing.Specification (
   Level, Border,
   TexturePosition1D(..), TexturePosition2D(..), TexturePosition3D(..),
   TextureSize1D(..), TextureSize2D(..), TextureSize3D(..),
   texImage1D, texImage2D, texImage3D,
   copyTexImage1D, copyTexImage2D,
   texSubImage1D, texSubImage2D, texSubImage3D,
   copyTexSubImage1D, copyTexSubImage2D, copyTexSubImage3D,
   CompressedTextureFormat(..), compressedTextureFormats,
   CompressedPixelData(..),
   compressedTexImage1D, compressedTexImage2D, compressedTexImage3D,
   compressedTexSubImage1D, compressedTexSubImage2D, compressedTexSubImage3D,
   getTexImage, getCompressedTexImage )

--------------------------------------------------------------------------------

-- ToDo: cube maps
texture :: TextureTarget -> StateVar Capability
texture = makeCapability . textureTargetToEnableCap

textureTargetToEnableCap :: TextureTarget -> EnableCap
textureTargetToEnableCap x = case x of
    Texture1D -> CapTexture1D
    Texture2D -> CapTexture2D
    Texture3D -> CapTexture3D
    TextureCubeMap -> CapTextureCubeMap
    TextureRectangle -> error "ToDo: TextureRectangle"
