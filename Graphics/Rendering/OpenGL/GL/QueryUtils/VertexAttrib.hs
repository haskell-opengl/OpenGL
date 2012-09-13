-- #hide
-----------------------------------------------------------------------------
--
-- Module      :  Graphics.Rendering.OpenGL.GL.QueryUtils.VertexAttrib
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :  Jason Dagit <dagitj@gmail.com>, Sven Panne <sven.panne@aedion.de>
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.QueryUtils.VertexAttrib (
    AttribLocation(..), GetVertexAttribPName(..),
    getVertexAttribInteger1, getVertexAttribEnum1, getVertexAttribBoolean1,
    getVertexAttribFloat4, getVertexAttribIInteger4, getVertexAttribIuInteger4,
    GetVertexAttribPointerPName(..), getVertexAttribPointer
) where

import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.OpenGL.GL.PeekPoke
import Graphics.Rendering.OpenGL.Raw.Core32

newtype AttribLocation = AttribLocation GLuint
   deriving ( Eq, Ord, Show )

--------------------------------------------------------------------------------

data GetVertexAttribPName =
     GetVertexAttribArrayEnabled
   | GetVertexAttribArraySize
   | GetVertexAttribArrayStride
   | GetVertexAttribArrayType
   | GetVertexAttribArrayNormalized
   | GetCurrentVertexAttrib
   | GetVertexAttribArrayBufferBinding
   | GetVertexAttribArrayInteger

marshalGetVertexAttribPName :: GetVertexAttribPName -> GLenum
marshalGetVertexAttribPName x = case x of
   GetVertexAttribArrayEnabled -> gl_VERTEX_ATTRIB_ARRAY_ENABLED
   GetVertexAttribArraySize -> gl_VERTEX_ATTRIB_ARRAY_SIZE
   GetVertexAttribArrayStride -> gl_VERTEX_ATTRIB_ARRAY_STRIDE
   GetVertexAttribArrayType -> gl_VERTEX_ATTRIB_ARRAY_TYPE
   GetVertexAttribArrayNormalized -> gl_VERTEX_ATTRIB_ARRAY_NORMALIZED
   GetCurrentVertexAttrib -> gl_CURRENT_VERTEX_ATTRIB
   GetVertexAttribArrayBufferBinding -> gl_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING
   GetVertexAttribArrayInteger -> gl_VERTEX_ATTRIB_ARRAY_INTEGER

--------------------------------------------------------------------------------

getVertexAttribInteger1 :: (GLint -> b) -> AttribLocation -> GetVertexAttribPName -> IO b
getVertexAttribInteger1 f (AttribLocation location) n = alloca $ \buf -> do
   glGetVertexAttribiv location (marshalGetVertexAttribPName n) buf
   peek1 f buf

getVertexAttribEnum1 :: (GLenum -> b) -> AttribLocation -> GetVertexAttribPName -> IO b
getVertexAttribEnum1 f = getVertexAttribInteger1 (f . fromIntegral)

getVertexAttribBoolean1 :: (GLboolean -> b) -> AttribLocation -> GetVertexAttribPName -> IO b
getVertexAttribBoolean1 f = getVertexAttribInteger1 (f . fromIntegral)

getVertexAttribFloat4 :: (GLfloat -> GLfloat -> GLfloat -> GLfloat -> b) -> AttribLocation -> GetVertexAttribPName -> IO b
getVertexAttribFloat4 f (AttribLocation location) n = alloca $ \buf -> do
   glGetVertexAttribfv location (marshalGetVertexAttribPName n) buf
   peek4 f buf

getVertexAttribIInteger4 :: (GLint -> GLint -> GLint -> GLint -> b) -> AttribLocation -> GetVertexAttribPName -> IO b
getVertexAttribIInteger4 f (AttribLocation location) n = alloca $ \buf -> do
   glGetVertexAttribIiv location (marshalGetVertexAttribPName n) buf
   peek4 f buf

getVertexAttribIuInteger4 :: (GLuint -> GLuint -> GLuint -> GLuint -> b) -> AttribLocation -> GetVertexAttribPName -> IO b
getVertexAttribIuInteger4 f (AttribLocation location) n = alloca $ \buf -> do
   glGetVertexAttribIuiv location (marshalGetVertexAttribPName n) buf
   peek4 f buf

--------------------------------------------------------------------------------

data GetVertexAttribPointerPName =
   VertexAttribArrayPointer

marshalGetVertexAttribPointerPName :: GetVertexAttribPointerPName -> GLenum
marshalGetVertexAttribPointerPName x = case x of
   VertexAttribArrayPointer -> gl_VERTEX_ATTRIB_ARRAY_POINTER

--------------------------------------------------------------------------------

getVertexAttribPointer :: AttribLocation -> GetVertexAttribPointerPName -> IO (Ptr a)
getVertexAttribPointer (AttribLocation location) n = alloca $ \buf -> do
   glGetVertexAttribPointerv location (marshalGetVertexAttribPointerPName n) buf
   peek buf
