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
   -- * Misc
   TextureTarget(..), marshalTextureTarget,
   PixelInternalFormat(..),

   -- * Texture Objects
   TextureObject, defaultTextureObject, textureBinding,
   textureResident, areTexturesResident,
   texturePriority, prioritizeTextures
) where

import Control.Monad ( liftM )
import Data.List ( partition, genericLength )
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Marshal.Array ( withArray, peekArray, allocaArray )
import Foreign.Ptr ( Ptr )
import Foreign.Storable ( Storable(peek) )
import Graphics.Rendering.OpenGL.GL.BasicTypes (
   GLint, GLuint, GLsizei, GLenum, GLfloat, GLclampf, GLdouble )
import Graphics.Rendering.OpenGL.GL.BufferObjects ( ObjectName(..) )
import Graphics.Rendering.OpenGL.GL.Extensions (
   FunPtr, unsafePerformIO, Invoker, getProcAddress )
import Graphics.Rendering.OpenGL.GL.GLboolean (
   GLboolean, marshalGLboolean, unmarshalGLboolean )
import Graphics.Rendering.OpenGL.GL.PeekPoke ( peek1 )
import Graphics.Rendering.OpenGL.GL.PixelInternalFormat (
   PixelInternalFormat(..) )
import Graphics.Rendering.OpenGL.GL.QueryUtils (
   GetPName(GetTextureBinding1D,GetTextureBinding2D,GetTextureBinding3D,
            GetTextureBindingCubeMap,GetTextureBindingRectangle),
   getEnum1)
import Graphics.Rendering.OpenGL.GL.StateVar ( StateVar, makeStateVar )

--------------------------------------------------------------------------------

#include "HsOpenGLExt.h"

--------------------------------------------------------------------------------

data TextureTarget =
     Texture1D
   | Texture2D
   | Texture3D
--   | ProxyTexture1D
--   | ProxyTexture2D
--   | ProxyTexture3D
   | TextureCubeMap
--   | ProxyTextureCubeMap
--   | TextureCubeMapPositiveX
--   | TextureCubeMapNegativeX
--   | TextureCubeMapPositiveY
--   | TextureCubeMapNegativeY
--   | TextureCubeMapPositiveZ
--   | TextureCubeMapNegativeZ
   | TextureRectangle
--   | ProxyTextureRectangle
   deriving ( Eq, Ord, Show )

marshalTextureTarget :: TextureTarget -> GLenum
marshalTextureTarget x = case x of
   Texture1D -> 0xde0
   Texture2D -> 0xde1
   Texture3D -> 0x806f
--   ProxyTexture1D -> 0x8063
--   ProxyTexture2D -> 0x8064
--   ProxyTexture3D -> 0x8070
   TextureCubeMap -> 0x8513
--   ProxyTextureCubeMap -> 0x851b
--   TextureCubeMapPositiveX -> 0x8515
--   TextureCubeMapNegativeX -> 0x8516
--   TextureCubeMapPositiveY -> 0x8517
--   TextureCubeMapNegativeY -> 0x8518
--   TextureCubeMapPositiveZ -> 0x8519
--   TextureCubeMapNegativeZ -> 0x851a
   TextureRectangle -> 0x84f5
--   ProxyTextureRectangle -> 0x84f7

textureTargetToGetPName :: TextureTarget -> GetPName
textureTargetToGetPName x = case x of
    Texture1D -> GetTextureBinding1D
    Texture2D -> GetTextureBinding2D
    Texture3D -> GetTextureBinding3D
    TextureCubeMap -> GetTextureBindingCubeMap
    TextureRectangle -> GetTextureBindingRectangle

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

data TexParameter =
     TextureMinFilter
   | TextureMagFilter
   | TextureWrapS
   | TextureWrapT
   | TextureWrapR
   | TextureBorderColor
   | TextureMinLod
   | TextureMaxLod
   | TextureBaseLevel
   | TextureMaxLevel
   | TexturePriority
   | TextureMaxAnisotropy
   | TextureCompare
   | TextureCompareOperator
   | TextureCompareFailValue
   | GenerateMipmap
   | TextureCompareMode
   | TextureCompareFunc
   | DepthTextureMode
   | TextureLodBias
   | TextureResident

marshalTexParameter :: TexParameter -> GLenum
marshalTexParameter x = case x of
   TextureMinFilter -> 0x2801
   TextureMagFilter -> 0x2800
   TextureWrapS -> 0x2802
   TextureWrapT -> 0x2803
   TextureWrapR -> 0x8072
   TextureBorderColor -> 0x1004
   TextureMinLod -> 0x813A
   TextureMaxLod -> 0x813B
   TextureBaseLevel -> 0x813C
   TextureMaxLevel -> 0x813D
   TexturePriority -> 0x8066
   TextureMaxAnisotropy -> 0x84FE
   TextureCompare -> 0x819A
   TextureCompareOperator -> 0x819B
   TextureCompareFailValue -> 0x80BF
   GenerateMipmap -> 0x8191
   TextureCompareMode -> 0x884C
   TextureCompareFunc -> 0x884D
   DepthTextureMode -> 0x884B
   TextureLodBias -> 0x8501
   TextureResident -> 0x8067

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "glTexParameterf"
   glTexParameterf :: GLenum -> GLenum ->  GLfloat -> IO ()

foreign import CALLCONV unsafe "glTexParameteri"
   glTexParameteri :: GLenum -> GLenum ->  GLint -> IO ()

foreign import CALLCONV unsafe "glTexParameterfv"
   glTexParameterfv :: GLenum -> GLenum -> Ptr GLfloat -> IO ()

foreign import CALLCONV unsafe "glTexParameteriv"
   glTexParameteriv :: GLenum -> GLenum -> Ptr GLint -> IO ()

getTexParameterf :: TextureTarget -> TexParameter -> IO GLfloat
getTexParameterf t p =
   alloca $ \buf -> do
     glGetTexParameterfv (marshalTextureTarget t) (marshalTexParameter p) buf
     peek buf

foreign import CALLCONV unsafe "glGetTexParameterfv"
   glGetTexParameterfv :: GLenum -> GLenum -> Ptr GLfloat -> IO ()

getTexParameteri :: (GLint -> a) -> TextureTarget -> TexParameter -> IO a
getTexParameteri f t p =
   alloca $ \buf -> do
     glGetTexParameteriv (marshalTextureTarget t) (marshalTexParameter p) buf
     peek1 f buf

foreign import CALLCONV unsafe "glGetTexParameteriv"
   glGetTexParameteriv :: GLenum -> GLenum -> Ptr GLint -> IO ()

--------------------------------------------------------------------------------

data TexLevelParameter =
     TextureComponents
   | TextureWidth
   | TextureHeight
   | TextureDepth
   | TextureInternalFormat
   | TextureBorder
   | TextureRedSize
   | TextureGreenSize
   | TextureBlueSize
   | TextureAlphaSize
   | TextureIntensitySize
   | TextureLuminanceSize
   | TextureIndexSize
   | DepthBits
   | TextureCompressedImageSize
   | TextureCompressed

marshalTexLevelParameter :: TexLevelParameter -> GLenum
marshalTexLevelParameter x = case x of
   TextureComponents -> 0x1003
   TextureWidth -> 0x1000
   TextureHeight -> 0x1001
   TextureDepth -> 0x8071
   TextureInternalFormat -> 0x1003
   TextureBorder -> 0x1005
   TextureRedSize -> 0x805C
   TextureGreenSize -> 0x805D
   TextureBlueSize -> 0x805E
   TextureAlphaSize -> 0x805F
   TextureIntensitySize -> 0x8061
   TextureLuminanceSize -> 0x8060
   TextureIndexSize -> 0x80ED
   DepthBits -> 0x0D56
   TextureCompressedImageSize -> 0x86A0
   TextureCompressed -> 0x86A1

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "glGetTexLevelParameterfv"
   glGetTexLevelParameterfv :: GLenum -> GLint -> GLenum -> Ptr GLfloat -> IO ()

-- all targets incl. proxies and all cube maps
foreign import CALLCONV unsafe "glGetTexLevelParameteriv"
   glGetTexLevelParameteriv :: GLenum -> GLint -> GLenum -> Ptr GLint -> IO ()

--------------------------------------------------------------------------------

newtype TextureObject = TextureObject { textureID :: GLuint }
   deriving ( Eq, Ord, Show )

defaultTextureObject :: TextureObject
defaultTextureObject = TextureObject 0

--------------------------------------------------------------------------------

instance ObjectName TextureObject where
   genObjectNames n =
      allocaArray n $ \buf -> do
        glGenTextures (fromIntegral n) buf
        liftM (map TextureObject) $ peekArray n buf

   deleteObjectNames textureObjects = do
      withArray (map textureID textureObjects) $
         glDeleteTextures (genericLength textureObjects)

   isObjectName = liftM unmarshalGLboolean . glIsTexture . textureID

foreign import CALLCONV unsafe "glGenTextures"
   glGenTextures :: GLsizei -> Ptr GLuint -> IO ()

foreign import CALLCONV unsafe "glDeleteTextures"
   glDeleteTextures :: GLsizei -> Ptr GLuint -> IO ()

foreign import CALLCONV unsafe "glIsTexture"
   glIsTexture :: GLuint -> IO GLboolean

--------------------------------------------------------------------------------

textureBinding :: TextureTarget -> StateVar TextureObject
textureBinding t =
   makeStateVar
      (getEnum1 TextureObject (textureTargetToGetPName t))
      (glBindTexture (marshalTextureTarget t) . textureID)

foreign import CALLCONV unsafe "glBindTexture"
   glBindTexture :: GLenum -> GLuint -> IO ()

--------------------------------------------------------------------------------

-- ToDo: allow proxies!
textureResident :: TextureTarget -> GettableStateVar Bool
textureResident t =
   makeGettableStateVar
      (getTexParameteri (unmarshalGLboolean . fromIntegral) t TextureResident)

areTexturesResident :: [TextureObject] -> IO ([TextureObject],[TextureObject])
areTexturesResident texObjs = do
   let len = length texObjs
   withArray (map textureID texObjs) $ \texObjsBuf ->
      allocaArray len $ \residentBuf -> do
         allResident <-
            glAreTexturesResident (fromIntegral len) texObjsBuf residentBuf
         if unmarshalGLboolean allResident
            then return (texObjs, [])
            else do
               tr <- liftM (zip texObjs) $ peekArray len residentBuf
               let (resident, nonResident) = partition (unmarshalGLboolean . snd) tr
               return (map fst resident, map fst nonResident)

foreign import CALLCONV unsafe "glAreTexturesResident"
   glAreTexturesResident :: GLsizei -> Ptr GLuint -> Ptr GLboolean -> IO GLboolean

--------------------------------------------------------------------------------

texturePriority :: TextureTarget -> StateVar GLclampf
texturePriority t =
   makeStateVar
      (getTexParameterf t TexturePriority)
      (glTexParameterf (marshalTextureTarget t) (marshalTexParameter TexturePriority))

prioritizeTextures :: [(TextureObject,GLclampf)] -> IO ()
prioritizeTextures tps =
   withArray (map (textureID . fst) tps) $ \texObjsBuf ->
      withArray (map snd tps) $
         glPrioritizeTextures (genericLength tps) texObjsBuf

foreign import CALLCONV unsafe "glPrioritizeTextures"
   glPrioritizeTextures :: GLsizei -> Ptr GLuint -> Ptr GLclampf -> IO ()

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "glTexEnvf"
   glTexEnvf :: GLenum -> GLenum ->  GLfloat -> IO ()

foreign import CALLCONV unsafe "glTexEnvi"
   glTexEnvi :: GLenum -> GLenum ->  GLint -> IO ()

foreign import CALLCONV unsafe "glTexEnvfv"
   glTexEnvfv :: GLenum -> GLenum -> Ptr GLfloat -> IO ()

foreign import CALLCONV unsafe "glTexEnviv"
   glTexEnviv :: GLenum -> GLenum -> Ptr GLint -> IO ()

foreign import CALLCONV unsafe "glGetTexEnvfv"
   glGetTexEnvfv :: GLenum -> GLenum -> Ptr GLfloat -> IO ()

foreign import CALLCONV unsafe "glGetTexEnviv"
   glGetTexEnviv :: GLenum -> GLenum -> Ptr GLint -> IO ()

--------------------------------------------------------------------------------

foreign import CALLCONV unsafe "glTexGend"
   glTexGend :: GLenum -> GLenum ->  GLdouble -> IO ()

foreign import CALLCONV unsafe "glTexGenf"
   glTexGenf :: GLenum -> GLenum ->  GLfloat -> IO ()

foreign import CALLCONV unsafe "glTexGeni"
   glTexGeni :: GLenum -> GLenum ->  GLint -> IO ()

foreign import CALLCONV unsafe "glTexGendv"
   glTexGendv :: GLenum -> GLenum -> Ptr GLdouble -> IO ()

foreign import CALLCONV unsafe "glTexGenfv"
   glTexGenfv :: GLenum -> GLenum -> Ptr GLfloat -> IO ()

foreign import CALLCONV unsafe "glTexGeniv"
   glTexGeniv :: GLenum -> GLenum -> Ptr GLint -> IO ()

foreign import CALLCONV unsafe "glGetTexGendv"
   glGetTexGendv :: GLenum -> GLenum -> Ptr GLdouble -> IO ()

foreign import CALLCONV unsafe "glGetTexGenfv"
   glGetTexGenfv :: GLenum -> GLenum -> Ptr GLfloat -> IO ()

foreign import CALLCONV unsafe "glGetTexGeniv"
   glGetTexGeniv :: GLenum -> GLenum -> Ptr GLint -> IO ()
