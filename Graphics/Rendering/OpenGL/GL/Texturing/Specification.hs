--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Texturing.Specification
-- Copyright   :  (c) Sven Panne 2002-2005
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
-- This module corresponds to section 3.8.1 (Texture Image Specification),
-- section 3.8.2 (Alternate Texture Image Specification Commands), and section
-- 3.8.3 (Compressed Texture Images) of the OpenGL 1.5 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Texturing.Specification (
   -- * Texture-related Data Types
   TextureTarget(..), CubeMapTarget(..), Level, Border,
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
   getCompressedTexImage
) where

import Foreign.Marshal.Array ( peekArray, allocaArray )
import Foreign.Ptr ( Ptr )
import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLint, GLsizei, GLenum )
import Graphics.Rendering.OpenGL.GL.CoordTrans ( Position(..) )
import Graphics.Rendering.OpenGL.GL.Extensions (
   FunPtr, unsafePerformIO, Invoker, getProcAddress )
import Graphics.Rendering.OpenGL.GL.PixelData ( withPixelData )
import Graphics.Rendering.OpenGL.GL.Texturing.PixelInternalFormat (
   marshalPixelInternalFormat, marshalPixelInternalFormat' )
import Graphics.Rendering.OpenGL.GL.PixelRectangles (
   PixelInternalFormat, PixelData, Proxy(..) )
import Graphics.Rendering.OpenGL.GL.QueryUtils (
   GetPName(GetNumCompressedTextureFormats,GetCompressedTextureFormats),
   getInteger1, getIntegerv)
import Graphics.Rendering.OpenGL.GL.StateVar (
   GettableStateVar, makeGettableStateVar )
import Graphics.Rendering.OpenGL.GL.Texturing.TextureTarget (
   TextureTarget(..), marshalTextureTarget, marshalProxyTextureTarget,
   CubeMapTarget(..), marshalCubeMapTarget )

--------------------------------------------------------------------------------

#include "HsOpenGLExt.h"

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

texImage1D :: Proxy -> Level -> PixelInternalFormat -> TextureSize1D -> Border -> PixelData a -> IO ()
texImage1D proxy level int (TextureSize1D w) border pd =
   withPixelData pd $
      glTexImage1D
         (marshalProxyTextureTarget proxy Texture1D)
         level (marshalPixelInternalFormat int) w border

foreign import CALLCONV unsafe "glTexImage1D"
   glTexImage1D :: GLenum -> GLint -> GLint -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr a -> IO ()

--------------------------------------------------------------------------------

texImage2D :: Maybe CubeMapTarget -> Proxy -> Level -> PixelInternalFormat -> TextureSize2D -> Border -> PixelData a -> IO ()
texImage2D mbCubeMap proxy level int (TextureSize2D w h) border pd =
   withPixelData pd $
      glTexImage2D
         (maybe (marshalProxyTextureTarget proxy Texture2D)
                (\c -> if proxy == Proxy then marshalProxyTextureTarget Proxy TextureCubeMap else marshalCubeMapTarget c)
                mbCubeMap)
         level (marshalPixelInternalFormat int) w h border

foreign import CALLCONV unsafe "glTexImage2D"
   glTexImage2D :: GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr a -> IO ()

--------------------------------------------------------------------------------

texImage3D :: Proxy -> Level -> PixelInternalFormat -> TextureSize3D -> Border -> PixelData a -> IO ()
texImage3D proxy level int (TextureSize3D w h d) border pd =
   withPixelData pd $
      glTexImage3DEXT
         (marshalProxyTextureTarget proxy Texture3D)
         level (marshalPixelInternalFormat int) w h d border

EXTENSION_ENTRY("GL_EXT_texture3D or OpenGL 1.2",glTexImage3DEXT,GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr a -> IO ())

--------------------------------------------------------------------------------

getTexImage :: Either TextureTarget CubeMapTarget -> Level -> PixelData a -> IO ()
getTexImage t level pd =
   withPixelData pd $
      glGetTexImage (either marshalTextureTarget marshalCubeMapTarget t) level

foreign import CALLCONV unsafe "glGetTexImage"
   glGetTexImage :: GLenum -> GLint -> GLenum -> GLenum -> Ptr a -> IO ()

--------------------------------------------------------------------------------

copyTexImage1D :: Level -> PixelInternalFormat -> Position -> TextureSize1D -> Border -> IO ()
copyTexImage1D level int (Position x y) (TextureSize1D w) border =
   glCopyTexImage1D
      (marshalTextureTarget Texture1D) level
      (marshalPixelInternalFormat' int) x y w border

foreign import CALLCONV unsafe "glCopyTexImage1D"
   glCopyTexImage1D :: GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLint -> IO ()

--------------------------------------------------------------------------------

copyTexImage2D :: Maybe CubeMapTarget -> Level -> PixelInternalFormat -> Position -> TextureSize2D -> Border -> IO ()
copyTexImage2D mbCubeMap level int (Position x y) (TextureSize2D w h) border =
   glCopyTexImage2D
      (maybe (marshalTextureTarget Texture2D) marshalCubeMapTarget mbCubeMap) level
      (marshalPixelInternalFormat' int) x y w h border

foreign import CALLCONV unsafe "glCopyTexImage2D"
   glCopyTexImage2D :: GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> IO ()

--------------------------------------------------------------------------------

texSubImage1D :: Level -> TexturePosition1D -> TextureSize1D -> PixelData a -> IO ()
texSubImage1D level (TexturePosition1D xOff) (TextureSize1D w) pd =
   withPixelData pd $
      glTexSubImage1D (marshalTextureTarget Texture1D) level xOff w

foreign import CALLCONV unsafe "glTexSubImage1D"
   glTexSubImage1D :: GLenum -> GLint -> GLint -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ()

--------------------------------------------------------------------------------

texSubImage2D :: Maybe CubeMapTarget -> Level -> TexturePosition2D -> TextureSize2D -> PixelData a -> IO ()
texSubImage2D mbCubeMap level (TexturePosition2D xOff yOff) (TextureSize2D w h) pd =
   withPixelData pd $
      glTexSubImage2D (maybe (marshalTextureTarget Texture2D) marshalCubeMapTarget mbCubeMap) level xOff yOff w h

foreign import CALLCONV unsafe "glTexSubImage2D"
   glTexSubImage2D :: GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ()

--------------------------------------------------------------------------------

texSubImage3D :: Level -> TexturePosition3D -> TextureSize3D -> PixelData a -> IO ()
texSubImage3D level (TexturePosition3D xOff yOff zOff) (TextureSize3D w h d) pd =
   withPixelData pd $
      glTexSubImage3DEXT (marshalTextureTarget Texture3D) level xOff yOff zOff w h d

EXTENSION_ENTRY("GL_EXT_texture3D or OpenGL 1.2",glTexSubImage3DEXT,GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())

--------------------------------------------------------------------------------

copyTexSubImage1D :: Level -> TexturePosition1D -> Position -> TextureSize1D -> IO ()
copyTexSubImage1D level (TexturePosition1D xOff) (Position x y) (TextureSize1D w) =
   glCopyTexSubImage1D (marshalTextureTarget Texture1D) level xOff x y w

foreign import CALLCONV unsafe "glCopyTexSubImage1D"
   glCopyTexSubImage1D :: GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> IO ()

--------------------------------------------------------------------------------

copyTexSubImage2D :: Maybe CubeMapTarget -> Level -> TexturePosition2D -> Position -> TextureSize2D -> IO ()
copyTexSubImage2D mbCubeMap level (TexturePosition2D xOff yOff) (Position x y) (TextureSize2D w h) =
   glCopyTexSubImage2D (maybe (marshalTextureTarget Texture2D) marshalCubeMapTarget mbCubeMap) level xOff yOff x y w h

foreign import CALLCONV unsafe "glCopyTexSubImage2D"
   glCopyTexSubImage2D :: GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ()

--------------------------------------------------------------------------------

copyTexSubImage3D :: Level -> TexturePosition3D -> Position -> TextureSize2D -> IO ()
copyTexSubImage3D level (TexturePosition3D xOff yOff zOff) (Position x y) (TextureSize2D w h) =
   glCopyTexSubImage3DEXT (marshalTextureTarget Texture3D) level xOff yOff zOff x y w h

EXTENSION_ENTRY("GL_EXT_texture3D or OpenGL 1.2",glCopyTexSubImage3DEXT,GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ())

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
#ifdef __HADDOCK__
-- Help Haddock a bit, because it doesn't do any instance inference.
instance Eq (CompressedPixelData a)
instance Ord (CompressedPixelData a)
instance Show (CompressedPixelData a)
#else
   deriving ( Eq, Ord, Show )
#endif

withCompressedPixelData ::
   CompressedPixelData a -> (GLenum -> GLsizei -> Ptr a -> b) -> b
withCompressedPixelData
   (CompressedPixelData (CompressedTextureFormat fmt) size ptr) f =
   f fmt size ptr

--------------------------------------------------------------------------------

compressedTexImage1D :: Proxy -> Level -> TextureSize1D -> Border -> CompressedPixelData a -> IO ()
compressedTexImage1D proxy level (TextureSize1D w) border cpd =
   withCompressedPixelData cpd $ \fmt ->
      glCompressedTexImage1DARB
         (marshalProxyTextureTarget proxy Texture1D) level fmt w border

EXTENSION_ENTRY("GL_ARB_texture_compression or OpenGL 1.3",glCompressedTexImage1DARB,GLenum -> GLint -> GLenum -> GLsizei -> GLint -> GLsizei -> Ptr a -> IO ())

--------------------------------------------------------------------------------

compressedTexImage2D :: Maybe CubeMapTarget -> Proxy -> Level -> TextureSize2D -> Border -> CompressedPixelData a -> IO ()
compressedTexImage2D mbCubeMap proxy level (TextureSize2D w h) border cpd =
   withCompressedPixelData cpd $ \fmt ->
      glCompressedTexImage2DARB
         (maybe (marshalProxyTextureTarget proxy Texture2D)
                (\c -> if proxy == Proxy then marshalProxyTextureTarget Proxy TextureCubeMap else marshalCubeMapTarget c)
                mbCubeMap)
         level fmt w h border

EXTENSION_ENTRY("GL_ARB_texture_compression or OpenGL 1.3",glCompressedTexImage2DARB,GLenum -> GLint -> GLenum -> GLsizei -> GLsizei ->GLint -> GLsizei -> Ptr a -> IO ())

--------------------------------------------------------------------------------

compressedTexImage3D :: Proxy -> Level -> TextureSize3D -> Border -> CompressedPixelData a -> IO ()
compressedTexImage3D proxy level (TextureSize3D w h d) border cpd =
   withCompressedPixelData cpd $ \fmt ->
      glCompressedTexImage3DARB
         (marshalProxyTextureTarget proxy Texture3D) level fmt w h d border

EXTENSION_ENTRY("GL_ARB_texture_compression or OpenGL 1.3",glCompressedTexImage3DARB,GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLsizei -> Ptr a -> IO ())

--------------------------------------------------------------------------------

getCompressedTexImage :: Either TextureTarget CubeMapTarget -> Level -> Ptr a -> IO ()
getCompressedTexImage = glGetCompressedTexImageARB . either marshalTextureTarget marshalCubeMapTarget

EXTENSION_ENTRY("GL_ARB_texture_compression or OpenGL 1.3",glGetCompressedTexImageARB,GLenum -> GLint -> Ptr a -> IO ())

--------------------------------------------------------------------------------

compressedTexSubImage1D :: Level -> TexturePosition1D -> TextureSize1D -> CompressedPixelData a -> IO ()
compressedTexSubImage1D level (TexturePosition1D xOff) (TextureSize1D w) cpd =
   withCompressedPixelData cpd $
      glCompressedTexSubImage1DARB (marshalTextureTarget Texture1D) level xOff w

EXTENSION_ENTRY("GL_ARB_texture_compression or OpenGL 1.3",glCompressedTexSubImage1DARB,GLenum -> GLint -> GLint -> GLsizei -> GLenum -> GLsizei -> Ptr a -> IO ())

--------------------------------------------------------------------------------

compressedTexSubImage2D :: Maybe CubeMapTarget -> Level -> TexturePosition2D -> TextureSize2D -> CompressedPixelData a -> IO ()
compressedTexSubImage2D mbCubeMap level (TexturePosition2D xOff yOff) (TextureSize2D w h) cpd =
   withCompressedPixelData cpd $
      glCompressedTexSubImage2DARB (maybe (marshalTextureTarget Texture2D) marshalCubeMapTarget mbCubeMap) level xOff yOff w h

EXTENSION_ENTRY("GL_ARB_texture_compression or OpenGL 1.3",glCompressedTexSubImage2DARB,GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr a-> IO ())

--------------------------------------------------------------------------------

compressedTexSubImage3D :: Level -> TexturePosition3D -> TextureSize3D -> CompressedPixelData a -> IO ()
compressedTexSubImage3D level (TexturePosition3D xOff yOff zOff) (TextureSize3D w h d) cpd =
   withCompressedPixelData cpd $
      glCompressedTexSubImage3DARB (marshalTextureTarget Texture3D) level xOff yOff zOff w h d

EXTENSION_ENTRY("GL_ARB_texture_compression or OpenGL 1.3",glCompressedTexSubImage3DARB,GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr a -> IO ())
