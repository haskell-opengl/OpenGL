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
   TextureTarget(..), marshalTextureTarget,
   PixelInternalFormat(..)
) where

import Foreign.Ptr ( Ptr )
import Graphics.Rendering.OpenGL.GL.BasicTypes (
   GLint, GLuint, GLsizei, GLenum, GLfloat, GLclampf, GLdouble )
import Graphics.Rendering.OpenGL.GL.Extensions (
   FunPtr, unsafePerformIO, Invoker, getProcAddress )
import Graphics.Rendering.OpenGL.GL.GLboolean ( GLboolean )
import Graphics.Rendering.OpenGL.GL.PixelInternalFormat (
   PixelInternalFormat(..) )

--------------------------------------------------------------------------------

#include "HsOpenGLExt.h"

--------------------------------------------------------------------------------

data TextureTarget =
     Texture1D
   | Texture2D
   | Texture3D
   | ProxyTexture1D
   | ProxyTexture2D
   | ProxyTexture3D
   | TextureCubeMap
   | ProxyTextureCubeMap
   | TextureCubeMapPositiveX
   | TextureCubeMapNegativeX
   | TextureCubeMapPositiveY
   | TextureCubeMapNegativeY
   | TextureCubeMapPositiveZ
   | TextureCubeMapNegativeZ
   deriving ( Eq, Ord, Show )

marshalTextureTarget :: TextureTarget -> GLenum
marshalTextureTarget x = case x of
   Texture1D -> 0xde0
   Texture2D -> 0xde1
   Texture3D -> 0x806f
   ProxyTexture1D -> 0x8063
   ProxyTexture2D -> 0x8064
   ProxyTexture3D -> 0x8070
   TextureCubeMap -> 0x8513
   ProxyTextureCubeMap -> 0x851b
   TextureCubeMapPositiveX -> 0x8515
   TextureCubeMapNegativeX -> 0x8516
   TextureCubeMapPositiveY -> 0x8517
   TextureCubeMapNegativeY -> 0x8518
   TextureCubeMapPositiveZ -> 0x8519
   TextureCubeMapNegativeZ -> 0x851a

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "glTexImage1D"
   glTexImage1D :: GLenum -> GLint -> GLint -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr a -> IO ()

foreign import CALLCONV unsafe "glTexImage2D"
   glTexImage2D :: GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr a -> IO ()

EXTENSION_ENTRY("GL_EXT_texture3D or OpenGL 1.2",glTexImage3DEXT,GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLenum -> GLenum -> Ptr a -> IO ())

foreign import CALLCONV unsafe "glGetTexImage"
   glGetTexImage :: GLenum -> GLint -> GLenum -> GLenum -> Ptr a -> IO ()

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "glCopyTexImage1D"
   glCopyTexImage1D :: GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLint -> IO ()

foreign import CALLCONV unsafe "glCopyTexImage2D"
   glCopyTexImage2D :: GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> IO ()

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "glTexSubImage1D"
   glTexSubImage1D :: GLenum -> GLint -> GLint -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ()

foreign import CALLCONV unsafe "glTexSubImage2D"
   glTexSubImage2D :: GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ()

EXTENSION_ENTRY("GL_EXT_texture3D or OpenGL 1.2",glTexSubImage3DEXT,GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "glCopyTexSubImage1D"
   glCopyTexSubImage1D :: GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> IO ()

foreign import CALLCONV unsafe "glCopyTexSubImage2D"
   glCopyTexSubImage2D :: GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ()

EXTENSION_ENTRY("GL_EXT_texture3D or OpenGL 1.2",glCopyTexSubImage3DEXT,GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ())

--------------------------------------------------------------------------------

EXTENSION_ENTRY("GL_ARB_texture_compression or OpenGL 1.3",glCompressedTexImage1DARB,GLenum -> GLint -> GLenum -> GLsizei -> GLint -> GLsizei -> Ptr a -> IO ())

EXTENSION_ENTRY("GL_ARB_texture_compression or OpenGL 1.3",glCompressedTexImage2DARB,GLenum -> GLint -> GLenum -> GLsizei -> GLsizei ->GLint -> GLsizei -> Ptr a -> IO ())

EXTENSION_ENTRY("GL_ARB_texture_compression or OpenGL 1.3",glCompressedTexImage3DARB,GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLsizei -> GLint -> GLsizei -> Ptr a -> IO ())

EXTENSION_ENTRY("GL_ARB_texture_compression or OpenGL 1.3",glGetCompressedTexImageARB,GLenum -> GLint -> Ptr a -> IO ())

--------------------------------------------------------------------------------

EXTENSION_ENTRY("GL_ARB_texture_compression or OpenGL 1.3",glCompressedTexSubImage1DARB,GLenum -> GLint -> GLint -> GLsizei -> GLenum -> GLsizei -> Ptr a -> IO ())

EXTENSION_ENTRY("GL_ARB_texture_compression or OpenGL 1.3",glCompressedTexSubImage2DARB,GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr a-> IO ())

EXTENSION_ENTRY("GL_ARB_texture_compression or OpenGL 1.3",glCompressedTexSubImage3DARB,GLenum -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLsizei -> GLenum -> GLsizei -> Ptr a -> IO ())

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "glTexParameterf"
   glTexParameterf :: GLenum -> GLenum ->  GLfloat -> IO ()

foreign import CALLCONV unsafe "glTexParameteri"
   glTexParameteri :: GLenum -> GLenum ->  GLint -> IO ()

foreign import CALLCONV unsafe "glTexParameterfv"
   glTexParameterfv :: GLenum -> GLenum ->  Ptr GLfloat -> IO ()

foreign import CALLCONV unsafe "glTexParameteriv"
   glTexParameteriv :: GLenum -> GLenum ->  Ptr GLint -> IO ()

foreign import CALLCONV unsafe "glGetTexParameterfv"
   glGetTexParameterfv :: GLenum -> GLenum ->  Ptr GLfloat -> IO ()

foreign import CALLCONV unsafe "glGetTexParameteriv"
   glGetTexParameteriv :: GLenum -> GLenum ->  Ptr GLint -> IO ()

foreign import CALLCONV unsafe "glGetTexLevelParameterfv"
   glGetTexLevelParameterfv :: GLenum -> GLint -> GLenum ->  Ptr GLfloat -> IO ()

foreign import CALLCONV unsafe "glGetTexLevelParameteriv"
   glGetTexLevelParameteriv :: GLenum -> GLint -> GLenum ->  Ptr GLint -> IO ()

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "glGenTextures"
   glGenTextures :: GLsizei -> Ptr GLuint -> IO ()

foreign import CALLCONV unsafe "glDeleteTextures"
   glDeleteTextures :: GLsizei -> Ptr GLuint -> IO ()

foreign import CALLCONV unsafe "glIsTexture"
   glIsTexture :: GLuint -> IO GLboolean

foreign import CALLCONV unsafe "glBindTexture"
   glBindTexture :: GLenum -> GLuint -> IO ()

foreign import CALLCONV unsafe "glAreTexturesResident"
   glAreTexturesResident :: GLsizei -> Ptr GLuint -> Ptr GLboolean -> IO GLboolean

foreign import CALLCONV unsafe "glPrioritizeTextures"
   glPrioritizeTextures :: GLsizei -> Ptr GLuint -> Ptr GLclampf -> IO ()

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "glTexEnvf"
   glTexEnvf :: GLenum -> GLenum ->  GLfloat -> IO ()

foreign import CALLCONV unsafe "glTexEnvi"
   glTexEnvi :: GLenum -> GLenum ->  GLint -> IO ()

foreign import CALLCONV unsafe "glTexEnvfv"
   glTexEnvfv :: GLenum -> GLenum ->  Ptr GLfloat -> IO ()

foreign import CALLCONV unsafe "glTexEnviv"
   glTexEnviv :: GLenum -> GLenum ->  Ptr GLint -> IO ()

foreign import CALLCONV unsafe "glGetTexEnvfv"
   glGetTexEnvfv :: GLenum -> GLenum ->  Ptr GLfloat -> IO ()

foreign import CALLCONV unsafe "glGetTexEnviv"
   glGetTexEnviv :: GLenum -> GLenum ->  Ptr GLint -> IO ()

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "glTexGend"
   glTexGend :: GLenum -> GLenum ->  GLdouble -> IO ()

foreign import CALLCONV unsafe "glTexGenf"
   glTexGenf :: GLenum -> GLenum ->  GLfloat -> IO ()

foreign import CALLCONV unsafe "glTexGeni"
   glTexGeni :: GLenum -> GLenum ->  GLint -> IO ()

foreign import CALLCONV unsafe "glTexGendv"
   glTexGendv :: GLenum -> GLenum ->  Ptr GLdouble -> IO ()

foreign import CALLCONV unsafe "glTexGenfv"
   glTexGenfv :: GLenum -> GLenum ->  Ptr GLfloat -> IO ()

foreign import CALLCONV unsafe "glTexGeniv"
   glTexGeniv :: GLenum -> GLenum ->  Ptr GLint -> IO ()

foreign import CALLCONV unsafe "glGetTexGendv"
   glGetTexGendv :: GLenum -> GLenum ->  Ptr GLdouble -> IO ()

foreign import CALLCONV unsafe "glGetTexGenfv"
   glGetTexGenfv :: GLenum -> GLenum ->  Ptr GLfloat -> IO ()

foreign import CALLCONV unsafe "glGetTexGeniv"
   glGetTexGeniv :: GLenum -> GLenum ->  Ptr GLint -> IO ()
