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
   TexParameter(..), texParami, texParamf, texParamC4f, getTexParameteri,
   combineTexParams, combineTexParamsMaybe
) where

import Control.Monad
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.OpenGL.GL.Capability
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

texParameter :: TextureTarget t
             => (GLenum -> GLenum -> b -> IO ())
             -> (a -> (b -> IO ()) -> IO ())
             -> t -> TexParameter -> a -> IO ()
texParameter glTexParameter marshalAct t p x =
   marshalAct x $
      glTexParameter (marshalTextureTarget t) (marshalTexParameter p)

--------------------------------------------------------------------------------

getTexParameter :: (Storable b, TextureTarget t)
                => (GLenum -> GLenum -> Ptr b -> IO ())
                -> (b -> a)
                -> t -> TexParameter -> IO a
getTexParameter glGetTexParameter unmarshal t p =
   alloca $ \buf -> do
     glGetTexParameter (marshalTextureTarget t) (marshalTexParameter p) buf
     peek1 unmarshal buf

--------------------------------------------------------------------------------

m2a :: (a -> b) -> a -> (b -> IO ()) -> IO ()
m2a marshal x act = act (marshal x)

texParami :: TextureTarget t =>
   (GLint -> a) -> (a -> GLint) -> TexParameter -> t -> StateVar a
texParami unmarshal marshal p t =
   makeStateVar
      (getTexParameter glGetTexParameteriv unmarshal     t p)
      (texParameter    glTexParameteri     (m2a marshal) t p)

texParamf :: TextureTarget t =>
   (GLfloat -> a) -> (a -> GLfloat) -> TexParameter -> t -> StateVar a
texParamf unmarshal marshal p t =
   makeStateVar
      (getTexParameter glGetTexParameterfv unmarshal     t p)
      (texParameter    glTexParameterf     (m2a marshal) t p)

texParamC4f :: TextureTarget t => TexParameter -> t -> StateVar (Color4 GLfloat)
texParamC4f p t =
   makeStateVar
      (getTexParameter glGetTexParameterC4f id   t p)
      (texParameter    glTexParameterC4f    with t p)

glTexParameterC4f :: GLenum -> GLenum -> Ptr (Color4 GLfloat) -> IO ()
glTexParameterC4f target pname ptr = glTexParameterfv target pname (castPtr ptr)

glGetTexParameterC4f :: GLenum -> GLenum -> Ptr (Color4 GLfloat) -> IO ()
glGetTexParameterC4f target pname ptr = glGetTexParameterfv target pname (castPtr ptr)

getTexParameteri :: TextureTarget t => (GLint -> a) -> t -> TexParameter -> IO a
getTexParameteri = getTexParameter glGetTexParameteriv

--------------------------------------------------------------------------------

combineTexParams :: TextureTarget t
                 => (t -> StateVar a)
                 -> (t -> StateVar b)
                 -> (t -> StateVar (a,b))
combineTexParams v w t =
   makeStateVar
      (liftM2 (,) (get (v t)) (get (w t)))
      (\(x,y) -> do v t $= x; w t $= y)

combineTexParamsMaybe :: TextureTarget t
                      => (t -> StateVar Capability)
                      -> (t -> StateVar a)
                      -> (t -> StateVar (Maybe a))
combineTexParamsMaybe enab val t =
   makeStateVar
      (do tcm <- get (enab t)
          case tcm of
             Disabled -> return Nothing
             Enabled -> fmap Just $ get (val t))
      (maybe (enab t $= Disabled)
             (\tcf -> do val t $= tcf
                         enab t $= Enabled))
