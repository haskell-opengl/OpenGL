-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Texturing.TexParameter
-- Copyright   :  (c) Sven Panne 2002-2004
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
   TexParameter(..),
   texParameteri, texParameterf, texParameterC4f,
   getTexParameteri, getTexParameterf, getTexParameterC4f
) where

import Foreign.Marshal.Alloc ( alloca )
import Foreign.Marshal.Utils ( with )
import Foreign.Ptr ( Ptr )
import Foreign.Storable ( Storable(peek) )
import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLint, GLenum, GLfloat )
import Graphics.Rendering.OpenGL.GL.PeekPoke ( peek1 )
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

texParameteri :: (a -> GLint) -> TextureTarget -> TexParameter -> a -> IO ()
texParameteri f t p =
   glTexParameteri (marshalTextureTarget t) (marshalTexParameter p) . f

foreign import CALLCONV unsafe "glTexParameteri"
   glTexParameteri :: GLenum -> GLenum ->  GLint -> IO ()

texParameterf :: TextureTarget -> TexParameter -> GLfloat -> IO ()
texParameterf t = glTexParameterf (marshalTextureTarget t) . marshalTexParameter

foreign import CALLCONV unsafe "glTexParameterf"
   glTexParameterf :: GLenum -> GLenum ->  GLfloat -> IO ()

texParameterC4f :: TextureTarget -> TexParameter -> Color4 GLfloat -> IO ()
texParameterC4f t p c =
   with c $
      glTexParameterC4f (marshalTextureTarget t) (marshalTexParameter p)

foreign import CALLCONV unsafe "glTexParameterfv"
   glTexParameterC4f :: GLenum -> GLenum -> Ptr (Color4 GLfloat) -> IO ()

--------------------------------------------------------------------------------

getTexParameteri :: (GLint -> a) -> TextureTarget -> TexParameter -> IO a
getTexParameteri f t p =
   alloca $ \buf -> do
     glGetTexParameteriv (marshalTextureTarget t) (marshalTexParameter p) buf
     peek1 f buf

foreign import CALLCONV unsafe "glGetTexParameteriv"
   glGetTexParameteriv :: GLenum -> GLenum -> Ptr GLint -> IO ()

getTexParameterf :: TextureTarget -> TexParameter -> IO GLfloat
getTexParameterf t p =
   alloca $ \buf -> do
     glGetTexParameterfv (marshalTextureTarget t) (marshalTexParameter p) buf
     peek buf

foreign import CALLCONV unsafe "glGetTexParameterfv"
   glGetTexParameterfv :: GLenum -> GLenum -> Ptr GLfloat -> IO ()

getTexParameterC4f :: TextureTarget -> TexParameter -> IO (Color4 GLfloat)
getTexParameterC4f t p =
   alloca $ \buf -> do
     glGetTexParameterC4f (marshalTextureTarget t) (marshalTexParameter p) buf
     peek buf

foreign import CALLCONV unsafe "glGetTexParameterfv"
   glGetTexParameterC4f :: GLenum -> GLenum -> Ptr (Color4 GLfloat) -> IO ()
