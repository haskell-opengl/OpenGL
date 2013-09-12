{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Texturing.TexParameter
-- Copyright   :  (c) Sven Panne 2002-2013
-- License     :  BSD3
-- 
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This is a purely internal module for getting\/setting texture parameters.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Texturing.TexParameter (
   TexParameter(..), texParami, texParamf, texParamC4f, getTexParameteri
) where

import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.OpenGL.GL.PeekPoke
import Graphics.Rendering.OpenGL.GL.StateVar
import Graphics.Rendering.OpenGL.GL.Texturing.TextureTarget
import Graphics.Rendering.OpenGL.GL.VertexSpec
import Graphics.Rendering.OpenGL.Raw

--------------------------------------------------------------------------------

data TexParameter =
     TextureMinFilter
   | TextureMagFilter
   | TextureWrapS
   | TextureWrapT
   | TextureWrapR
   | TextureBorderColor
   | TextureMinLOD
   | TextureMaxLOD
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
   | TextureLODBias
   | TextureResident

marshalTexParameter :: TexParameter -> GLenum
marshalTexParameter x = case x of
   TextureMinFilter -> gl_TEXTURE_MIN_FILTER
   TextureMagFilter -> gl_TEXTURE_MAG_FILTER
   TextureWrapS -> gl_TEXTURE_WRAP_S
   TextureWrapT -> gl_TEXTURE_WRAP_T
   TextureWrapR -> gl_TEXTURE_WRAP_R
   TextureBorderColor -> gl_TEXTURE_BORDER_COLOR
   TextureMinLOD -> gl_TEXTURE_MIN_LOD
   TextureMaxLOD -> gl_TEXTURE_MAX_LOD
   TextureBaseLevel -> gl_TEXTURE_BASE_LEVEL
   TextureMaxLevel -> gl_TEXTURE_MAX_LEVEL
   TexturePriority -> gl_TEXTURE_PRIORITY
   TextureMaxAnisotropy -> gl_TEXTURE_MAX_ANISOTROPY
   TextureCompare -> 0x819A
   TextureCompareOperator -> 0x819B
   TextureCompareFailValue -> gl_TEXTURE_COMPARE_FAIL_VALUE
   GenerateMipmap -> gl_GENERATE_MIPMAP
   TextureCompareMode -> gl_TEXTURE_COMPARE_MODE
   TextureCompareFunc -> gl_TEXTURE_COMPARE_FUNC
   DepthTextureMode -> gl_DEPTH_TEXTURE_MODE
   TextureLODBias -> gl_TEXTURE_LOD_BIAS
   TextureResident -> gl_TEXTURE_RESIDENT

--------------------------------------------------------------------------------

{-
  Allowed targets:

  TEXTURE_1D
  ------------------------------------------------------------------------------
  TEXTURE_2D
  TEXTURE_2D_MULTISAMPLE
  TEXTURE_1D_ARRAY
  TEXTURE_RECTANGLE
  TEXTURE_CUBE_MAP
  ------------------------------------------------------------------------------
  TEXTURE_3D
  TEXTURE_2D_ARRAY
  TEXTURE_2D_MULTISAMPLE_ARRAY
  TEXTURE_CUBE_MAP_ARRAY

  In a nutshell: All non-proxy targets are allowed.
-}

texParameter :: ParameterizedTextureTarget t
             => (GLenum -> GLenum -> b -> IO ())
             -> (a -> (b -> IO ()) -> IO ())
             -> t -> TexParameter -> a -> IO ()
texParameter glTexParameter marshalAct t p x =
   marshalAct x $
      glTexParameter (marshalParameterizedTextureTarget t) (marshalTexParameter p)

--------------------------------------------------------------------------------

{-
  Allowed targets:

  TEXTURE_1D
  ------------------------------------------------------------------------------
  TEXTURE_2D
  TEXTURE_2D_MULTISAMPLE
  TEXTURE_1D_ARRAY
  TEXTURE_RECTANGLE
  TEXTURE_CUBE_MAP
  ------------------------------------------------------------------------------
  TEXTURE_3D
  TEXTURE_2D_ARRAY
  TEXTURE_2D_MULTISAMPLE_ARRAY
  TEXTURE_CUBE_MAP_ARRAY

  In a nutshell: All non-proxy targets are allowed.
-}

getTexParameter :: (Storable b, ParameterizedTextureTarget t)
                => (GLenum -> GLenum -> Ptr b -> IO ())
                -> (b -> a)
                -> t -> TexParameter -> IO a
getTexParameter glGetTexParameter unmarshal t p =
   alloca $ \buf -> do
     glGetTexParameter (marshalParameterizedTextureTarget t) (marshalTexParameter p) buf
     peek1 unmarshal buf

--------------------------------------------------------------------------------

m2a :: (a -> b) -> a -> (b -> IO ()) -> IO ()
m2a marshal x act = act (marshal x)

texParami :: ParameterizedTextureTarget t =>
   (GLint -> a) -> (a -> GLint) -> TexParameter -> t -> StateVar a
texParami unmarshal marshal p t =
   makeStateVar
      (getTexParameter glGetTexParameteriv unmarshal     t p)
      (texParameter    glTexParameteri     (m2a marshal) t p)

texParamf :: ParameterizedTextureTarget t =>
   (GLfloat -> a) -> (a -> GLfloat) -> TexParameter -> t -> StateVar a
texParamf unmarshal marshal p t =
   makeStateVar
      (getTexParameter glGetTexParameterfv unmarshal     t p)
      (texParameter    glTexParameterf     (m2a marshal) t p)

texParamC4f :: ParameterizedTextureTarget t => TexParameter -> t -> StateVar (Color4 GLfloat)
texParamC4f p t =
   makeStateVar
      (getTexParameter glGetTexParameterC4f id   t p)
      (texParameter    glTexParameterC4f    with t p)

glTexParameterC4f :: GLenum -> GLenum -> Ptr (Color4 GLfloat) -> IO ()
glTexParameterC4f target pname ptr = glTexParameterfv target pname (castPtr ptr)

glGetTexParameterC4f :: GLenum -> GLenum -> Ptr (Color4 GLfloat) -> IO ()
glGetTexParameterC4f target pname ptr = glGetTexParameterfv target pname (castPtr ptr)

getTexParameteri :: ParameterizedTextureTarget t => (GLint -> a) -> t -> TexParameter -> IO a
getTexParameteri = getTexParameter glGetTexParameteriv
