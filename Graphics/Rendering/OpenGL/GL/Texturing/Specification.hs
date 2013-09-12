--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Texturing.Specification
-- Copyright   :  (c) Sven Panne 2002-2013
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to section 3.8.1 (Texture Image Specification),
-- section 3.8.2 (Alternate Texture Image Specification Commands), and section
-- 3.8.3 (Compressed Texture Images) of the OpenGL 2.1 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Texturing.Specification (
   -- * Texture Targets

   -- ** One-Dimensional Texture Targets
   TextureTarget1D(..),

   -- ** Two-Dimensional Texture Targets
   TextureTarget2D(..),
   TextureTarget2DMultisample(..),
   TextureTargetCubeMap(..),
   TextureTargetCubeMapFace(..),

   -- ** Three-Dimensional Texture Targets
   TextureTarget3D(..),
   TextureTarget2DMultisampleArray(..),

   -- ** Texture Buffer Target
   TextureTargetBuffer(..),

   -- ** Texture Target Classification
   BindableTextureTarget,
   TextureTargetCompleteWithMultisample,
   TextureTargetSingleWithMultisample,
   TextureTargetSingleWithoutMultisample,

   -- * Texture-related Data Types
   Level, Border,
   TexturePosition1D(..), TexturePosition2D(..), TexturePosition3D(..),
   TextureSize1D(..), TextureSize2D(..), TextureSize3D(..),

   -- * Texture Image Specification
   texImage1D, texImage2D, texImage3D,
   copyTexImage1D, copyTexImage2D,
   texSubImage1D, texSubImage2D, texSubImage3D,
   getTexImage,

   -- * Alternate Texture Image Specification Commands
   copyTexSubImage1D, copyTexSubImage2D, copyTexSubImage3D,

   -- * Compressed Texture Images
   CompressedTextureFormat(..), compressedTextureFormats,
   CompressedPixelData(..),
   compressedTexImage1D, compressedTexImage2D, compressedTexImage3D,
   compressedTexSubImage1D, compressedTexSubImage2D, compressedTexSubImage3D,
   getCompressedTexImage,

  -- * Implementation-Dependent Limits
  maxTextureSize, maxCubeMapTextureSize, maxRectangleTextureSize,
  max3DTextureSize, maxArrayTextureLayers
) where

import Foreign.Ptr
import Graphics.Rendering.OpenGL.GL.CoordTrans
import Graphics.Rendering.OpenGL.GL.PixelData
import Graphics.Rendering.OpenGL.GL.PixelRectangles
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.GL.StateVar
import Graphics.Rendering.OpenGL.GL.Texturing.PixelInternalFormat
import Graphics.Rendering.OpenGL.GL.Texturing.TextureTarget
import Graphics.Rendering.OpenGL.Raw

--------------------------------------------------------------------------------

type Level = GLint

type Border = GLint

newtype TexturePosition1D = TexturePosition1D GLint
   deriving ( Eq, Ord, Show )

data TexturePosition2D = TexturePosition2D !GLint !GLint
   deriving ( Eq, Ord, Show )

data TexturePosition3D = TexturePosition3D !GLint !GLint !GLint
   deriving ( Eq, Ord, Show )

newtype TextureSize1D = TextureSize1D GLsizei
   deriving ( Eq, Ord, Show )

data TextureSize2D = TextureSize2D !GLsizei !GLsizei
   deriving ( Eq, Ord, Show )

data TextureSize3D = TextureSize3D !GLsizei !GLsizei !GLsizei
   deriving ( Eq, Ord, Show )

--------------------------------------------------------------------------------

{-
  Allowed targets:

  TEXTURE_1D
  PROXY_TEXTURE_1D
-}

texImage1D :: TextureTarget1D -> Proxy -> Level -> PixelInternalFormat -> TextureSize1D -> Border -> PixelData a -> IO ()
texImage1D target proxy level int (TextureSize1D w) border pd =
   withPixelData pd $
      glTexImage1D
         (marshalTextureTargetSingleWithoutMultisample proxy target)
         level (marshalPixelInternalFormat int) w border

--------------------------------------------------------------------------------
{-
  Allowed targets:

  TEXTURE_2D
  TEXTURE_1D_ARRAY
  TEXTURE_RECTANGLE
  TEXTURE_CUBE_MAP_POSITIVE_X
  TEXTURE_CUBE_MAP_NEGATIVE_X
  TEXTURE_CUBE_MAP_POSITIVE_Y
  TEXTURE_CUBE_MAP_NEGATIVE_Y
  TEXTURE_CUBE_MAP_POSITIVE_Z
  TEXTURE_CUBE_MAP_NEGATIVE_Z
  PROXY_TEXTURE_2D
  PROXY_TEXTURE_1D_ARRAY
  PROXY_TEXTURE_RECTANGLE
  PROXY_TEXTURE_CUBE_MAP

  Note: No TEXTURE_2D_MULTISAMPLE or PROXY_TEXTURE_2D_MULTISAMPLE targets! For
  these use glTexImage2DMultisample.
-}

-- TODO: Cube map!!!!!!!!!!!!!!!!!!!!!!!!
texImage2D :: TextureTarget2D -> Proxy -> Level -> PixelInternalFormat -> TextureSize2D -> Border -> PixelData a -> IO ()
texImage2D target proxy level int (TextureSize2D w h) border pd = do
   let t = case proxy of
              NoProxy -> marshalTextureTargetSingleWithoutMultisample NoProxy target
              Proxy -> marshalTextureTargetCompleteWithMultisample Proxy target
   withPixelData pd $
      glTexImage2D t level (marshalPixelInternalFormat int) w h border

--------------------------------------------------------------------------------

{-
  Allowed targets:

  TEXTURE_3D
  TEXTURE_2D_ARRAY
  TEXTURE_CUBE_MAP_ARRAY ???????
  PROXY_TEXTURE_3D
  PROXY_TEXTURE_2D_ARRAY
  PROXY_TEXTURE_CUBE_MAP_ARRAY ???????

  Note: No TEXTURE_2D_MULTISAMPLE_ARRAY or PROXY_TEXTURE_2D_MULTISAMPLE_ARRAY
  targets! For these use glTexImage3DMultisample.
-}

texImage3D :: TextureTarget3D -> Proxy -> Level -> PixelInternalFormat -> TextureSize3D -> Border -> PixelData a -> IO ()
texImage3D target proxy level int (TextureSize3D w h d) border pd =
   withPixelData pd $
      glTexImage3D
         (marshalTextureTargetSingleWithoutMultisample proxy target)
         level (marshalPixelInternalFormat int) w h d border

--------------------------------------------------------------------------------

{-
  Allowed targets:

  TEXTURE_1D
  ------------------------------------------------------------------------------
  TEXTURE_2D
  TEXTURE_1D_ARRAY
  TEXTURE_RECTANGLE
  TEXTURE_CUBE_MAP_POSITIVE_X
  TEXTURE_CUBE_MAP_NEGATIVE_X
  TEXTURE_CUBE_MAP_POSITIVE_Y
  TEXTURE_CUBE_MAP_NEGATIVE_Y
  TEXTURE_CUBE_MAP_POSITIVE_Z
  TEXTURE_CUBE_MAP_NEGATIVE_Z
  ------------------------------------------------------------------------------
  TEXTURE_3D
  TEXTURE_2D_ARRAY
  TEXTURE_CUBE_MAP_ARRAY
-}

getTexImage :: TextureTargetSingleWithoutMultisample t => t -> Level -> PixelData a -> IO ()
getTexImage target level pd =
   withPixelData pd $
      glGetTexImage (marshalTextureTargetSingleWithoutMultisample NoProxy target) level

--------------------------------------------------------------------------------

-- see texImage1D, but no proxies
copyTexImage1D :: TextureTarget1D -> Level -> PixelInternalFormat -> Position -> TextureSize1D -> Border -> IO ()
copyTexImage1D target level int (Position x y) (TextureSize1D w) border =
   glCopyTexImage1D
      (marshalTextureTargetSingleWithoutMultisample NoProxy target) level
      (marshalPixelInternalFormat' int) x y w border

--------------------------------------------------------------------------------

-- see texImage2D, but no proxies
copyTexImage2D :: TextureTarget2D -> Level -> PixelInternalFormat -> Position -> TextureSize2D -> Border -> IO ()
copyTexImage2D target level int (Position x y) (TextureSize2D w h) border =
   glCopyTexImage2D
      (marshalTextureTargetSingleWithoutMultisample NoProxy target) level
      (marshalPixelInternalFormat' int) x y w h border

--------------------------------------------------------------------------------

-- see texImage1D, but no proxies
texSubImage1D :: TextureTarget1D -> Level -> TexturePosition1D -> TextureSize1D -> PixelData a -> IO ()
texSubImage1D target level (TexturePosition1D xOff) (TextureSize1D w) pd =
   withPixelData pd $
      glTexSubImage1D (marshalTextureTargetSingleWithoutMultisample NoProxy target) level xOff w

--------------------------------------------------------------------------------

-- see texImage2D, but no proxies
texSubImage2D :: TextureTarget2D -> Level -> TexturePosition2D -> TextureSize2D -> PixelData a -> IO ()
texSubImage2D target level (TexturePosition2D xOff yOff) (TextureSize2D w h) pd =
   withPixelData pd $
      glTexSubImage2D (marshalTextureTargetSingleWithoutMultisample NoProxy target) level xOff yOff w h

--------------------------------------------------------------------------------

-- see texImage3D, but no proxies
texSubImage3D :: TextureTarget3D -> Level -> TexturePosition3D -> TextureSize3D -> PixelData a -> IO ()
texSubImage3D target level (TexturePosition3D xOff yOff zOff) (TextureSize3D w h d) pd =
   withPixelData pd $
      glTexSubImage3D (marshalTextureTargetSingleWithoutMultisample NoProxy target) level xOff yOff zOff w h d

--------------------------------------------------------------------------------

-- see texImage1D, but no proxies
copyTexSubImage1D :: TextureTarget1D -> Level -> TexturePosition1D -> Position -> TextureSize1D -> IO ()
copyTexSubImage1D target level (TexturePosition1D xOff) (Position x y) (TextureSize1D w) =
   glCopyTexSubImage1D (marshalTextureTargetSingleWithoutMultisample NoProxy target) level xOff x y w

--------------------------------------------------------------------------------

-- see texImage2D, but no proxies
copyTexSubImage2D :: TextureTarget2D -> Level -> TexturePosition2D -> Position -> TextureSize2D -> IO ()
copyTexSubImage2D target level (TexturePosition2D xOff yOff) (Position x y) (TextureSize2D w h) =
   glCopyTexSubImage2D (marshalTextureTargetSingleWithoutMultisample NoProxy target) level xOff yOff x y w h

--------------------------------------------------------------------------------

-- see texImage3D, but no proxies
copyTexSubImage3D :: TextureTarget3D -> Level -> TexturePosition3D -> Position -> TextureSize2D -> IO ()
copyTexSubImage3D target level (TexturePosition3D xOff yOff zOff) (Position x y) (TextureSize2D w h) =
   glCopyTexSubImage3D (marshalTextureTargetSingleWithoutMultisample NoProxy target) level xOff yOff zOff x y w h

--------------------------------------------------------------------------------

newtype CompressedTextureFormat = CompressedTextureFormat GLenum
   deriving ( Eq, Ord, Show )

compressedTextureFormats :: GettableStateVar [CompressedTextureFormat]
compressedTextureFormats =
   makeGettableStateVar $ do
      n <- getInteger1 fromIntegral GetNumCompressedTextureFormats
      getEnumN CompressedTextureFormat GetCompressedTextureFormats n

--------------------------------------------------------------------------------

data CompressedPixelData a =
     CompressedPixelData !CompressedTextureFormat GLsizei (Ptr a)
   deriving ( Eq, Ord, Show )

withCompressedPixelData ::
   CompressedPixelData a -> (GLenum -> GLsizei -> Ptr a -> b) -> b
withCompressedPixelData
   (CompressedPixelData (CompressedTextureFormat fmt) size ptr) f =
   f fmt size ptr

--------------------------------------------------------------------------------

-- see texImage1D
compressedTexImage1D :: TextureTarget1D -> Proxy -> Level -> TextureSize1D -> Border -> CompressedPixelData a -> IO ()
compressedTexImage1D target proxy level (TextureSize1D w) border cpd =
   withCompressedPixelData cpd $ \fmt ->
      glCompressedTexImage1D
         (marshalTextureTargetSingleWithoutMultisample proxy target) level fmt w border

--------------------------------------------------------------------------------

-- see texImage2D (but no rectangle texture formats)
compressedTexImage2D :: TextureTarget2D -> Proxy -> Level -> TextureSize2D -> Border -> CompressedPixelData a -> IO ()
compressedTexImage2D target proxy level (TextureSize2D w h) border cpd =
   withCompressedPixelData cpd $ \fmt ->
      glCompressedTexImage2D
         (marshalTextureTargetSingleWithoutMultisample proxy target)
         level fmt w h border

--------------------------------------------------------------------------------

-- see texImage3D
compressedTexImage3D :: TextureTarget3D -> Proxy -> Level -> TextureSize3D -> Border -> CompressedPixelData a -> IO ()
compressedTexImage3D target proxy level (TextureSize3D w h d) border cpd =
   withCompressedPixelData cpd $ \fmt ->
      glCompressedTexImage3D
         (marshalTextureTargetSingleWithoutMultisample proxy target) level fmt w h d border

--------------------------------------------------------------------------------

getCompressedTexImage :: TextureTargetSingleWithoutMultisample t => t -> Level -> Ptr a -> IO ()
getCompressedTexImage = glGetCompressedTexImage . marshalTextureTargetSingleWithoutMultisample NoProxy

--------------------------------------------------------------------------------

-- see texImage1D, but no proxies
compressedTexSubImage1D :: TextureTarget1D -> Level -> TexturePosition1D -> TextureSize1D -> CompressedPixelData a -> IO ()
compressedTexSubImage1D target level (TexturePosition1D xOff) (TextureSize1D w) cpd =
   withCompressedPixelData cpd $
      glCompressedTexSubImage1D (marshalTextureTargetSingleWithoutMultisample NoProxy target) level xOff w

--------------------------------------------------------------------------------

-- see texImage2D, but no proxies
compressedTexSubImage2D :: TextureTarget2D -> Level -> TexturePosition2D -> TextureSize2D -> CompressedPixelData a -> IO ()
compressedTexSubImage2D target  level (TexturePosition2D xOff yOff) (TextureSize2D w h) cpd =
   withCompressedPixelData cpd $
      glCompressedTexSubImage2D (marshalTextureTargetSingleWithoutMultisample NoProxy target) level xOff yOff w h

--------------------------------------------------------------------------------

-- see texImage3D, but no proxies
compressedTexSubImage3D :: TextureTarget3D -> Level -> TexturePosition3D -> TextureSize3D -> CompressedPixelData a -> IO ()
compressedTexSubImage3D target level (TexturePosition3D xOff yOff zOff) (TextureSize3D w h d) cpd =
   withCompressedPixelData cpd $
      glCompressedTexSubImage3D (marshalTextureTargetSingleWithoutMultisample NoProxy target) level xOff yOff zOff w h d

--------------------------------------------------------------------------------

maxTextureSize :: GettableStateVar GLsizei
maxTextureSize = maxTextureSizeWith GetMaxTextureSize

maxCubeMapTextureSize :: GettableStateVar GLsizei
maxCubeMapTextureSize = maxTextureSizeWith GetMaxCubeMapTextureSize

maxRectangleTextureSize :: GettableStateVar GLsizei
maxRectangleTextureSize = maxTextureSizeWith GetMaxRectangleTextureSize

max3DTextureSize :: GettableStateVar GLsizei
max3DTextureSize = maxTextureSizeWith GetMax3DTextureSize

maxArrayTextureLayers :: GettableStateVar GLsizei
maxArrayTextureLayers = maxTextureSizeWith GetMaxArrayTextureLayers

maxTextureSizeWith :: PName1I -> GettableStateVar GLsizei
maxTextureSizeWith = makeGettableStateVar . getInteger1 fromIntegral
