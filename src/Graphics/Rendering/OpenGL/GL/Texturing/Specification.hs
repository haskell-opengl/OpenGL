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
   ParameterizedTextureTarget,
   OneDimensionalTextureTarget,
   TwoDimensionalTextureTarget,
   ThreeDimensionalTextureTarget,
   QueryableTextureTarget,
   GettableTextureTarget,

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

   -- * Multisample Texture Images
   SampleLocations(..), texImage2DMultisample, texImage3DMultisample,

   -- * Implementation-Dependent Limits
   maxTextureSize, maxCubeMapTextureSize, maxRectangleTextureSize,
   max3DTextureSize, maxArrayTextureLayers, maxSampleMaskWords,
   maxColorTextureSamples, maxDepthTextureSamples, maxIntegerSamples
) where

import Foreign.Ptr
import Data.StateVar
import Graphics.Rendering.OpenGL.GL.CoordTrans
import Graphics.Rendering.OpenGL.GL.FramebufferObjects.RenderbufferTarget
import Graphics.Rendering.OpenGL.GL.GLboolean
import Graphics.Rendering.OpenGL.GL.PixelData
import Graphics.Rendering.OpenGL.GL.PixelRectangles
import Graphics.Rendering.OpenGL.GL.QueryUtils
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

texImage1D :: OneDimensionalTextureTarget t => t -> Proxy -> Level -> PixelInternalFormat -> TextureSize1D -> Border -> PixelData a -> IO ()
texImage1D target proxy level int (TextureSize1D w) border pd =
   withPixelData pd $
      glTexImage1D
         (marshalOneDimensionalTextureTarget proxy target)
         level (marshalPixelInternalFormat int) w border

--------------------------------------------------------------------------------

texImage2D :: TwoDimensionalTextureTarget t => t -> Proxy -> Level -> PixelInternalFormat -> TextureSize2D -> Border -> PixelData a -> IO ()
texImage2D target proxy level int (TextureSize2D w h) border pd =
   withPixelData pd $
      glTexImage2D (marshalTwoDimensionalTextureTarget proxy target) level (marshalPixelInternalFormat int) w h border

--------------------------------------------------------------------------------

texImage3D :: ThreeDimensionalTextureTarget t => t -> Proxy -> Level -> PixelInternalFormat -> TextureSize3D -> Border -> PixelData a -> IO ()
texImage3D target proxy level int (TextureSize3D w h d) border pd =
   withPixelData pd $
      glTexImage3D
         (marshalThreeDimensionalTextureTarget proxy target)
         level (marshalPixelInternalFormat int) w h d border

--------------------------------------------------------------------------------

getTexImage :: GettableTextureTarget t => t -> Level -> PixelData a -> IO ()
getTexImage target level pd =
   withPixelData pd $
      glGetTexImage (marshalGettableTextureTarget target) level

--------------------------------------------------------------------------------

copyTexImage1D :: OneDimensionalTextureTarget t => t -> Level -> PixelInternalFormat -> Position -> TextureSize1D -> Border -> IO ()
copyTexImage1D target level int (Position x y) (TextureSize1D w) border =
   glCopyTexImage1D
      (marshalOneDimensionalTextureTarget NoProxy target) level
      (marshalPixelInternalFormat' int) x y w border

--------------------------------------------------------------------------------

copyTexImage2D :: TwoDimensionalTextureTarget t => t -> Level -> PixelInternalFormat -> Position -> TextureSize2D -> Border -> IO ()
copyTexImage2D target level int (Position x y) (TextureSize2D w h) border =
   glCopyTexImage2D
      (marshalTwoDimensionalTextureTarget NoProxy target) level
      (marshalPixelInternalFormat' int) x y w h border

--------------------------------------------------------------------------------

texSubImage1D :: OneDimensionalTextureTarget t => t -> Level -> TexturePosition1D -> TextureSize1D -> PixelData a -> IO ()
texSubImage1D target level (TexturePosition1D xOff) (TextureSize1D w) pd =
   withPixelData pd $
      glTexSubImage1D (marshalOneDimensionalTextureTarget NoProxy target) level xOff w

--------------------------------------------------------------------------------

texSubImage2D :: TwoDimensionalTextureTarget t => t -> Level -> TexturePosition2D -> TextureSize2D -> PixelData a -> IO ()
texSubImage2D target level (TexturePosition2D xOff yOff) (TextureSize2D w h) pd =
   withPixelData pd $
      glTexSubImage2D (marshalTwoDimensionalTextureTarget NoProxy target) level xOff yOff w h

--------------------------------------------------------------------------------

texSubImage3D :: ThreeDimensionalTextureTarget t => t -> Level -> TexturePosition3D -> TextureSize3D -> PixelData a -> IO ()
texSubImage3D target level (TexturePosition3D xOff yOff zOff) (TextureSize3D w h d) pd =
   withPixelData pd $
      glTexSubImage3D (marshalThreeDimensionalTextureTarget NoProxy target) level xOff yOff zOff w h d

--------------------------------------------------------------------------------

copyTexSubImage1D :: OneDimensionalTextureTarget t => t -> Level -> TexturePosition1D -> Position -> TextureSize1D -> IO ()
copyTexSubImage1D target level (TexturePosition1D xOff) (Position x y) (TextureSize1D w) =
   glCopyTexSubImage1D (marshalOneDimensionalTextureTarget NoProxy target) level xOff x y w

--------------------------------------------------------------------------------

copyTexSubImage2D :: TwoDimensionalTextureTarget t => t -> Level -> TexturePosition2D -> Position -> TextureSize2D -> IO ()
copyTexSubImage2D target level (TexturePosition2D xOff yOff) (Position x y) (TextureSize2D w h) =
   glCopyTexSubImage2D (marshalTwoDimensionalTextureTarget NoProxy target) level xOff yOff x y w h

--------------------------------------------------------------------------------

copyTexSubImage3D :: ThreeDimensionalTextureTarget t => t -> Level -> TexturePosition3D -> Position -> TextureSize2D -> IO ()
copyTexSubImage3D target level (TexturePosition3D xOff yOff zOff) (Position x y) (TextureSize2D w h) =
   glCopyTexSubImage3D (marshalThreeDimensionalTextureTarget NoProxy target) level xOff yOff zOff x y w h

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

compressedTexImage1D :: OneDimensionalTextureTarget t => t -> Proxy -> Level -> TextureSize1D -> Border -> CompressedPixelData a -> IO ()
compressedTexImage1D target proxy level (TextureSize1D w) border cpd =
   withCompressedPixelData cpd $ \fmt ->
      glCompressedTexImage1D
         (marshalOneDimensionalTextureTarget proxy target) level fmt w border

--------------------------------------------------------------------------------

-- Note that the spec currently disallows TextureRectangle, but then again the
-- extension specification explicitly allows a relaxation in the future.
compressedTexImage2D :: TwoDimensionalTextureTarget t => t -> Proxy -> Level -> TextureSize2D -> Border -> CompressedPixelData a -> IO ()
compressedTexImage2D target proxy level (TextureSize2D w h) border cpd =
   withCompressedPixelData cpd $ \fmt ->
      glCompressedTexImage2D (marshalTwoDimensionalTextureTarget proxy target) level fmt w h border

--------------------------------------------------------------------------------

compressedTexImage3D :: ThreeDimensionalTextureTarget t => t -> Proxy -> Level -> TextureSize3D -> Border -> CompressedPixelData a -> IO ()
compressedTexImage3D target proxy level (TextureSize3D w h d) border cpd =
   withCompressedPixelData cpd $ \fmt ->
      glCompressedTexImage3D
         (marshalThreeDimensionalTextureTarget proxy target) level fmt w h d border

--------------------------------------------------------------------------------

getCompressedTexImage :: GettableTextureTarget t => t -> Level -> Ptr a -> IO ()
getCompressedTexImage = glGetCompressedTexImage . marshalGettableTextureTarget

--------------------------------------------------------------------------------

compressedTexSubImage1D :: OneDimensionalTextureTarget t => t -> Level -> TexturePosition1D -> TextureSize1D -> CompressedPixelData a -> IO ()
compressedTexSubImage1D target level (TexturePosition1D xOff) (TextureSize1D w) cpd =
   withCompressedPixelData cpd $
      glCompressedTexSubImage1D (marshalOneDimensionalTextureTarget NoProxy target) level xOff w

--------------------------------------------------------------------------------

compressedTexSubImage2D :: TwoDimensionalTextureTarget t => t -> Level -> TexturePosition2D -> TextureSize2D -> CompressedPixelData a -> IO ()
compressedTexSubImage2D target  level (TexturePosition2D xOff yOff) (TextureSize2D w h) cpd =
   withCompressedPixelData cpd $
      glCompressedTexSubImage2D (marshalTwoDimensionalTextureTarget NoProxy target) level xOff yOff w h

--------------------------------------------------------------------------------

-- see texImage3D, but no proxies
compressedTexSubImage3D :: ThreeDimensionalTextureTarget t => t -> Level -> TexturePosition3D -> TextureSize3D -> CompressedPixelData a -> IO ()
compressedTexSubImage3D target level (TexturePosition3D xOff yOff zOff) (TextureSize3D w h d) cpd =
   withCompressedPixelData cpd $
      glCompressedTexSubImage3D (marshalThreeDimensionalTextureTarget NoProxy target) level xOff yOff zOff w h d

--------------------------------------------------------------------------------

data SampleLocations =
     FlexibleSampleLocations
   | FixedSampleLocations
   deriving ( Eq, Ord, Show )

marshalSampleLocations :: SampleLocations -> GLboolean
marshalSampleLocations = marshalGLboolean . (FixedSampleLocations ==)

{-
unmarshalSampleLocations :: GLboolean -> SampleLocations
unmarshalSampleLocations x =
   if unmarshalGLboolean x
      then FixedSampleLocations
      else FlexibleSampleLocations
-}

--------------------------------------------------------------------------------

texImage2DMultisample :: TextureTarget2DMultisample
                      -> Proxy
                      -> Samples
                      -> PixelInternalFormat
                      -> TextureSize2D
                      -> SampleLocations
                      -> IO ()
texImage2DMultisample target proxy (Samples s) int (TextureSize2D w h) loc =
   glTexImage2DMultisample
      (marshalMultisample proxy target) s (fromIntegral (marshalPixelInternalFormat int))
      w h (marshalSampleLocations loc)

marshalMultisample :: ParameterizedTextureTarget t => Proxy -> t -> GLenum
marshalMultisample proxy = case proxy of
   NoProxy -> marshalParameterizedTextureTarget
   Proxy -> marshalParameterizedTextureTargetProxy

texImage3DMultisample :: TextureTarget2DMultisampleArray
                      -> Proxy
                      -> Samples
                      -> PixelInternalFormat
                      -> TextureSize3D
                      -> SampleLocations
                      -> IO ()
texImage3DMultisample target proxy (Samples s) int (TextureSize3D w h d) loc =
   glTexImage3DMultisample
      (marshalMultisample proxy target) s (fromIntegral (marshalPixelInternalFormat int))
      w h d (marshalSampleLocations loc)

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

maxSampleMaskWords :: GettableStateVar GLsizei
maxSampleMaskWords = maxTextureSizeWith GetMaxSampleMaskWords

maxColorTextureSamples :: GettableStateVar GLsizei
maxColorTextureSamples = maxTextureSizeWith GetMaxColorTextureSamples

maxDepthTextureSamples :: GettableStateVar GLsizei
maxDepthTextureSamples = maxTextureSizeWith GetMaxDepthTextureSamples

maxIntegerSamples :: GettableStateVar GLsizei
maxIntegerSamples = maxTextureSizeWith GetMaxIntegerSamples

maxTextureSizeWith :: PName1I -> GettableStateVar GLsizei
maxTextureSizeWith = makeGettableStateVar . getInteger1 fromIntegral
