--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Texturing.Specification
-- Copyright   :  (c) Sven Panne 2002-2009
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
--
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to section 3.8.1 (Texture Image Specification),
-- section 3.8.2 (Alternate Texture Image Specification Commands), and section
-- 3.8.3 (Compressed Texture Images) of the OpenGL 2.1 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Texturing.Specification (
   -- * Texture-related Data Types
   TextureTarget(), CubeMapTarget(..), TextureTarget1D(..), TextureTarget2D(..), TextureTarget3D(..),
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
  maxTextureSize
) where

import Data.StateVar
import Foreign.Marshal.Array
import Foreign.Ptr
import Graphics.Rendering.OpenGL.GL.CoordTrans
import Graphics.Rendering.OpenGL.GL.PixelData
import Graphics.Rendering.OpenGL.GL.PixelRectangles
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.GL.Texturing.PixelInternalFormat
import Graphics.Rendering.OpenGL.GL.Texturing.TextureTarget
import Graphics.Rendering.OpenGL.Raw.Core31

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

texImage1D :: TextureTarget1D -> Proxy -> Level -> PixelInternalFormat -> TextureSize1D -> Border -> PixelData a -> IO ()
texImage1D target proxy level int (TextureSize1D w) border pd =
   withPixelData pd $
      glTexImage1D
         (marshalProxyTextureTarget proxy target)
         level (marshalPixelInternalFormat int) w border

--------------------------------------------------------------------------------

texImage2D :: TextureTarget2D -> Proxy -> Level -> PixelInternalFormat -> TextureSize2D -> Border -> PixelData a -> IO ()
texImage2D target proxy level int (TextureSize2D w h) border pd =
   withPixelData pd $
      glTexImage2D
         (marshalProxyTextureTarget proxy target)
         level (marshalPixelInternalFormat int) w h border

--------------------------------------------------------------------------------

texImage3D :: TextureTarget3D -> Proxy -> Level -> PixelInternalFormat -> TextureSize3D -> Border -> PixelData a -> IO ()
texImage3D target proxy level int (TextureSize3D w h d) border pd =
   withPixelData pd $
      glTexImage3D
         (marshalProxyTextureTarget proxy target)
         level (marshalPixelInternalFormat int) w h d border

--------------------------------------------------------------------------------

getTexImage :: TextureTarget t =>  t -> Level -> PixelData a -> IO ()
getTexImage target level pd =
   withPixelData pd $
      glGetTexImage (marshalTextureTarget target) level

--------------------------------------------------------------------------------

copyTexImage1D :: TextureTarget1D -> Level -> PixelInternalFormat -> Position -> TextureSize1D -> Border -> IO ()
copyTexImage1D target level int (Position x y) (TextureSize1D w) border =
   glCopyTexImage1D
      (marshalTextureTarget target) level
      (marshalPixelInternalFormat' int) x y w border

--------------------------------------------------------------------------------

copyTexImage2D :: TextureTarget2D -> Level -> PixelInternalFormat -> Position -> TextureSize2D -> Border -> IO ()
copyTexImage2D target level int (Position x y) (TextureSize2D w h) border =
   glCopyTexImage2D
      (marshalTextureTarget target) level
      (marshalPixelInternalFormat' int) x y w h border

--------------------------------------------------------------------------------

texSubImage1D :: TextureTarget1D -> Level -> TexturePosition1D -> TextureSize1D -> PixelData a -> IO ()
texSubImage1D target level (TexturePosition1D xOff) (TextureSize1D w) pd =
   withPixelData pd $
      glTexSubImage1D (marshalTextureTarget target) level xOff w

--------------------------------------------------------------------------------

texSubImage2D :: TextureTarget2D -> Level -> TexturePosition2D -> TextureSize2D -> PixelData a -> IO ()
texSubImage2D target level (TexturePosition2D xOff yOff) (TextureSize2D w h) pd =
   withPixelData pd $
      glTexSubImage2D (marshalTextureTarget target) level xOff yOff w h

--------------------------------------------------------------------------------

texSubImage3D :: TextureTarget3D -> Level -> TexturePosition3D -> TextureSize3D -> PixelData a -> IO ()
texSubImage3D target level (TexturePosition3D xOff yOff zOff) (TextureSize3D w h d) pd =
   withPixelData pd $
      glTexSubImage3D (marshalTextureTarget target) level xOff yOff zOff w h d

--------------------------------------------------------------------------------

copyTexSubImage1D :: TextureTarget1D -> Level -> TexturePosition1D -> Position -> TextureSize1D -> IO ()
copyTexSubImage1D target level (TexturePosition1D xOff) (Position x y) (TextureSize1D w) =
   glCopyTexSubImage1D (marshalTextureTarget target) level xOff x y w

--------------------------------------------------------------------------------

copyTexSubImage2D :: TextureTarget2D -> Level -> TexturePosition2D -> Position -> TextureSize2D -> IO ()
copyTexSubImage2D target level (TexturePosition2D xOff yOff) (Position x y) (TextureSize2D w h) =
   glCopyTexSubImage2D (marshalTextureTarget target) level xOff yOff x y w h

--------------------------------------------------------------------------------

copyTexSubImage3D :: TextureTarget3D -> Level -> TexturePosition3D -> Position -> TextureSize2D -> IO ()
copyTexSubImage3D target level (TexturePosition3D xOff yOff zOff) (Position x y) (TextureSize2D w h) =
   glCopyTexSubImage3D (marshalTextureTarget target) level xOff yOff zOff x y w h

--------------------------------------------------------------------------------

newtype CompressedTextureFormat = CompressedTextureFormat GLenum
   deriving ( Eq, Ord, Show )

compressedTextureFormats :: GettableStateVar [CompressedTextureFormat]
compressedTextureFormats =
   makeGettableStateVar $ do
      n <- getInteger1 fromIntegral GetNumCompressedTextureFormats
      allocaArray n $ \buf -> do
         getIntegerv GetCompressedTextureFormats buf
         fmap (map (CompressedTextureFormat . fromIntegral)) $ peekArray n buf

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

compressedTexImage1D :: TextureTarget1D -> Proxy -> Level -> TextureSize1D -> Border -> CompressedPixelData a -> IO ()
compressedTexImage1D target proxy level (TextureSize1D w) border cpd =
   withCompressedPixelData cpd $ \fmt ->
      glCompressedTexImage1D
         (marshalProxyTextureTarget proxy target) level fmt w border

--------------------------------------------------------------------------------

compressedTexImage2D :: TextureTarget2D -> Proxy -> Level -> TextureSize2D -> Border -> CompressedPixelData a -> IO ()
compressedTexImage2D target proxy level (TextureSize2D w h) border cpd =
   withCompressedPixelData cpd $ \fmt ->
      glCompressedTexImage2D
         (marshalProxyTextureTarget proxy target)
         level fmt w h border

--------------------------------------------------------------------------------

compressedTexImage3D :: TextureTarget3D -> Proxy -> Level -> TextureSize3D -> Border -> CompressedPixelData a -> IO ()
compressedTexImage3D target proxy level (TextureSize3D w h d) border cpd =
   withCompressedPixelData cpd $ \fmt ->
      glCompressedTexImage3D
         (marshalProxyTextureTarget proxy target) level fmt w h d border

--------------------------------------------------------------------------------

getCompressedTexImage :: TextureTarget t => t -> Level -> Ptr a -> IO ()
getCompressedTexImage = glGetCompressedTexImage . marshalTextureTarget

--------------------------------------------------------------------------------

compressedTexSubImage1D :: TextureTarget1D -> Level -> TexturePosition1D -> TextureSize1D -> CompressedPixelData a -> IO ()
compressedTexSubImage1D target level (TexturePosition1D xOff) (TextureSize1D w) cpd =
   withCompressedPixelData cpd $
      glCompressedTexSubImage1D (marshalTextureTarget target) level xOff w

--------------------------------------------------------------------------------

compressedTexSubImage2D :: TextureTarget2D -> Level -> TexturePosition2D -> TextureSize2D -> CompressedPixelData a -> IO ()
compressedTexSubImage2D target level (TexturePosition2D xOff yOff) (TextureSize2D w h) cpd =
   withCompressedPixelData cpd $
      glCompressedTexSubImage2D (marshalTextureTarget target) level xOff yOff w h

--------------------------------------------------------------------------------

compressedTexSubImage3D :: TextureTarget3D -> Level -> TexturePosition3D -> TextureSize3D -> CompressedPixelData a -> IO ()
compressedTexSubImage3D target level (TexturePosition3D xOff yOff zOff) (TextureSize3D w h d) cpd =
   withCompressedPixelData cpd $
      glCompressedTexSubImage3D (marshalTextureTarget target) level xOff yOff zOff w h d

--------------------------------------------------------------------------------

maxTextureSize :: TextureTarget t => t -> GettableStateVar GLsizei
maxTextureSize = makeGettableStateVar . getInteger1 fromIntegral . textureTargetToMaxQuery
