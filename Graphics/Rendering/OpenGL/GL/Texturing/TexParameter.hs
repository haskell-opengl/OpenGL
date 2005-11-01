-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Texturing.TexParameter
-- Copyright   :  (c) Sven Panne 2002-2005
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
-- This is a purely internal module for getting\/setting texture parameters.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Texturing.TexParameter (
   TexParameter(..), texParami, texParamf, texParamC4f, getTexParameteri,
   combineTexParams, combineTexParamsMaybe
) where

import Control.Monad ( liftM2 )
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Marshal.Utils ( with )
import Foreign.Storable ( Storable )
import Foreign.Ptr ( Ptr )
import Graphics.Rendering.OpenGL.GL.BasicTypes (
   GLint, GLenum, GLfloat, Capability(..) )
import Graphics.Rendering.OpenGL.GL.PeekPoke ( peek1 )
import Graphics.Rendering.OpenGL.GL.StateVar (
   HasGetter(get), HasSetter(($=)), StateVar, makeStateVar )
import Graphics.Rendering.OpenGL.GL.Texturing.TextureTarget (
   TextureTarget(..), marshalTextureTarget )
import Graphics.Rendering.OpenGL.GL.VertexSpec( Color4(..) )

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
   TextureMinFilter -> 0x2801
   TextureMagFilter -> 0x2800
   TextureWrapS -> 0x2802
   TextureWrapT -> 0x2803
   TextureWrapR -> 0x8072
   TextureBorderColor -> 0x1004
   TextureMinLOD -> 0x813A
   TextureMaxLOD -> 0x813B
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
   TextureLODBias -> 0x8501
   TextureResident -> 0x8067

--------------------------------------------------------------------------------

texParameter :: (GLenum -> GLenum -> b -> IO ())
             -> (a -> (b -> IO ()) -> IO ())
             -> TextureTarget -> TexParameter -> a -> IO ()
texParameter glTexParameter marshalAct t p x =
   marshalAct x $
      glTexParameter (marshalTextureTarget t) (marshalTexParameter p)

foreign import CALLCONV unsafe "glTexParameteri"
   glTexParameteri :: GLenum -> GLenum ->  GLint -> IO ()

foreign import CALLCONV unsafe "glTexParameterf"
   glTexParameterf :: GLenum -> GLenum ->  GLfloat -> IO ()

foreign import CALLCONV unsafe "glTexParameterfv"
   glTexParameterC4f :: GLenum -> GLenum -> Ptr (Color4 GLfloat) -> IO ()

--------------------------------------------------------------------------------

getTexParameter :: Storable b
                => (GLenum -> GLenum -> Ptr b -> IO ())
                -> (b -> a)
                -> TextureTarget -> TexParameter -> IO a
getTexParameter glGetTexParameter unmarshal t p =
   alloca $ \buf -> do
     glGetTexParameter (marshalTextureTarget t) (marshalTexParameter p) buf
     peek1 unmarshal buf

foreign import CALLCONV unsafe "glGetTexParameteriv"
   glGetTexParameteriv :: GLenum -> GLenum -> Ptr GLint -> IO ()

foreign import CALLCONV unsafe "glGetTexParameterfv"
   glGetTexParameterfv :: GLenum -> GLenum -> Ptr GLfloat -> IO ()

foreign import CALLCONV unsafe "glGetTexParameterfv"
   glGetTexParameterC4f :: GLenum -> GLenum -> Ptr (Color4 GLfloat) -> IO ()

--------------------------------------------------------------------------------

m2a :: (a -> b) -> a -> (b -> IO ()) -> IO ()
m2a marshal x act = act (marshal x)

texParami ::
   (GLint -> a) -> (a -> GLint) -> TexParameter -> TextureTarget -> StateVar a
texParami unmarshal marshal p t =
   makeStateVar
      (getTexParameter glGetTexParameteriv unmarshal     t p)
      (texParameter    glTexParameteri     (m2a marshal) t p)

texParamf ::
   (GLfloat -> a) -> (a -> GLfloat) -> TexParameter -> TextureTarget -> StateVar a
texParamf unmarshal marshal p t =
   makeStateVar
      (getTexParameter glGetTexParameterfv unmarshal     t p)
      (texParameter    glTexParameterf     (m2a marshal) t p)

texParamC4f :: TexParameter -> TextureTarget -> StateVar (Color4 GLfloat)
texParamC4f p t =
   makeStateVar
      (getTexParameter glGetTexParameterC4f id   t p)
      (texParameter    glTexParameterC4f    with t p)

getTexParameteri :: (GLint -> a) -> TextureTarget -> TexParameter -> IO a
getTexParameteri = getTexParameter glGetTexParameteriv

--------------------------------------------------------------------------------

combineTexParams :: (TextureTarget -> StateVar a)
                 -> (TextureTarget -> StateVar b)
                 -> (TextureTarget -> StateVar (a,b))
combineTexParams v w t =
   makeStateVar
      (liftM2 (,) (get (v t)) (get (w t)))
      (\(x,y) -> do v t $= x; w t $= y)

combineTexParamsMaybe :: (TextureTarget -> StateVar Capability)
                      -> (TextureTarget -> StateVar a)
                      -> (TextureTarget -> StateVar (Maybe a))
combineTexParamsMaybe enab val t =
   makeStateVar
      (do tcm <- get (enab t)
          case tcm of
             Disabled -> return Nothing
             Enabled -> fmap Just $ get (val t))
      (maybe (enab t $= Disabled)
             (\tcf -> do val t $= tcf
                         enab t $= Enabled))
