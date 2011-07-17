--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.BufferObjects
-- Copyright   :  (c) Sven Panne 2002-2009
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
--
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to section 2.9 (Buffer Objects) of the OpenGL 2.1
-- specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.BufferObjects (
   -- * Object Names
   module Data.ObjectName,

   -- * Buffer Objects
   BufferObject(BufferObject),

   -- * Binding Buffer Objects
   BufferTarget(..), bindBuffer, arrayBufferBinding,
   vertexAttribArrayBufferBinding,

   -- * Handling Buffer Data
   BufferUsage(..), bufferData, TransferDirection(..), bufferSubData,

   -- * Mapping Buffer Objects
   BufferAccess(..), MappingFailure(..), withMappedBuffer,
   mapBuffer, unmapBuffer,
   bufferAccess, bufferMapped
) where

import Data.ObjectName
import Data.StateVar
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.OpenGL.GL.Exception
import Graphics.Rendering.OpenGL.GL.GLboolean
import Graphics.Rendering.OpenGL.GL.PeekPoke
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.GL.VertexArrays
import Graphics.Rendering.OpenGL.GLU.ErrorsInternal
import Graphics.Rendering.OpenGL.Raw.Core31

--------------------------------------------------------------------------------

newtype BufferObject = BufferObject { bufferID :: GLuint }
   deriving ( Eq, Ord, Show )

--------------------------------------------------------------------------------

instance ObjectName BufferObject where
   genObjectNames n =
      allocaArray n $ \buf -> do
        glGenBuffers (fromIntegral n) buf
        fmap (map BufferObject) $ peekArray n buf

   deleteObjectNames bufferObjects =
      withArrayLen (map bufferID bufferObjects) $
         glDeleteBuffers . fromIntegral

   isObjectName = fmap unmarshalGLboolean . glIsBuffer . bufferID

--------------------------------------------------------------------------------

data BufferTarget =
     ArrayBuffer
   | CopyReadBuffer
   | CopyWriteBuffer
   | ElementArrayBuffer
   | PixelPackBuffer
   | PixelUnpackBuffer
   deriving ( Eq, Ord, Show )

marshalBufferTarget :: BufferTarget -> GLenum
marshalBufferTarget x = case x of
   ArrayBuffer -> gl_ARRAY_BUFFER
   CopyReadBuffer -> gl_COPY_READ_BUFFER
   CopyWriteBuffer -> gl_COPY_WRITE_BUFFER
   ElementArrayBuffer -> gl_ELEMENT_ARRAY_BUFFER
   PixelPackBuffer -> gl_PIXEL_PACK_BUFFER
   PixelUnpackBuffer -> gl_PIXEL_UNPACK_BUFFER

bufferTargetToGetPName :: BufferTarget -> GetPName
bufferTargetToGetPName x = case x of
   ArrayBuffer -> GetArrayBufferBinding
   ElementArrayBuffer -> GetElementArrayBufferBinding
   CopyReadBuffer -> GetCopyReadBuffer
   CopyWriteBuffer -> GetCopyWriteBuffer
   PixelPackBuffer -> GetPixelPackBufferBinding
   PixelUnpackBuffer -> GetPixelUnpackBufferBinding

--------------------------------------------------------------------------------

data BufferUsage =
     StreamDraw
   | StreamRead
   | StreamCopy
   | StaticDraw
   | StaticRead
   | StaticCopy
   | DynamicDraw
   | DynamicRead
   | DynamicCopy
   deriving ( Eq, Ord, Show )

marshalBufferUsage :: BufferUsage -> GLenum
marshalBufferUsage x = case x of
   StreamDraw -> gl_STREAM_DRAW
   StreamRead -> gl_STREAM_READ
   StreamCopy -> gl_STREAM_COPY
   StaticDraw -> gl_STATIC_DRAW
   StaticRead -> gl_STATIC_READ
   StaticCopy -> gl_STATIC_COPY
   DynamicDraw -> gl_DYNAMIC_DRAW
   DynamicRead -> gl_DYNAMIC_READ
   DynamicCopy -> gl_DYNAMIC_COPY

unmarshalBufferUsage :: GLenum -> BufferUsage
unmarshalBufferUsage x
   | x == gl_STREAM_DRAW = StreamDraw
   | x == gl_STREAM_READ = StreamRead
   | x == gl_STREAM_COPY = StreamCopy
   | x == gl_STATIC_DRAW = StaticDraw
   | x == gl_STATIC_READ = StaticRead
   | x == gl_STATIC_COPY = StaticCopy
   | x == gl_DYNAMIC_DRAW = DynamicDraw
   | x == gl_DYNAMIC_READ = DynamicRead
   | x == gl_DYNAMIC_COPY = DynamicCopy
   | otherwise = error ("unmarshalBufferUsage: illegal value " ++ show x)

--------------------------------------------------------------------------------

data BufferAccess =
     ReadOnly
   | WriteOnly
   | ReadWrite
   deriving ( Eq, Ord, Show )

marshalBufferAccess :: BufferAccess -> GLenum
marshalBufferAccess x = case x of
   ReadOnly -> gl_READ_ONLY
   WriteOnly -> gl_WRITE_ONLY
   ReadWrite -> gl_READ_WRITE

unmarshalBufferAccess :: GLenum -> BufferAccess
unmarshalBufferAccess x
   | x == gl_READ_ONLY = ReadOnly
   | x == gl_WRITE_ONLY = WriteOnly
   | x == gl_READ_WRITE = ReadWrite
   | otherwise = error ("unmarshalBufferAccess: illegal value " ++ show x)

--------------------------------------------------------------------------------

bindBuffer :: BufferTarget -> StateVar (Maybe BufferObject)
bindBuffer t = makeStateVar (getBindBuffer t) (setBindBuffer t)

getBindBuffer :: BufferTarget -> IO (Maybe BufferObject)
getBindBuffer = bufferQuery bufferTargetToGetPName

bufferQuery :: (a -> GetPName) -> a -> IO (Maybe BufferObject)
bufferQuery func t = do
   buf <- getInteger1 (BufferObject . fromIntegral) (func t)
   return $ if buf == noBufferObject then Nothing else Just buf

noBufferObject :: BufferObject
noBufferObject = BufferObject 0

setBindBuffer :: BufferTarget -> Maybe BufferObject -> IO ()
setBindBuffer t =
   glBindBuffer (marshalBufferTarget t) . bufferID . maybe noBufferObject id

clientArrayTypeToGetPName :: ClientArrayType -> GetPName
clientArrayTypeToGetPName x = case x of
   VertexArray -> GetVertexArrayBufferBinding
   NormalArray -> GetNormalArrayBufferBinding
   ColorArray -> GetColorArrayBufferBinding
   IndexArray -> GetIndexArrayBufferBinding
   TextureCoordArray -> GetTextureCoordArrayBufferBinding
   EdgeFlagArray -> GetEdgeFlagArrayBufferBinding
   FogCoordArray -> GetFogCoordArrayBufferBinding
   SecondaryColorArray -> GetSecondaryColorArrayBufferBinding
   MatrixIndexArray -> error "clientArrayTypeToGetPName: impossible"

arrayBufferBinding :: ClientArrayType -> GettableStateVar (Maybe BufferObject)
arrayBufferBinding t =
   makeGettableStateVar $ case t of
      MatrixIndexArray -> do recordInvalidEnum ; return Nothing
      _ -> bufferQuery clientArrayTypeToGetPName t


vertexAttribArrayBufferBinding :: AttribLocation -> GettableStateVar (Maybe BufferObject)
vertexAttribArrayBufferBinding location =
   makeGettableStateVar $ do
      buf <- getVertexAttribInteger1 (BufferObject . fromIntegral) location GetVertexAttribArrayBufferBinding
      return $ if buf == noBufferObject then Nothing else Just buf

--------------------------------------------------------------------------------

bufferData :: BufferTarget -> StateVar (GLsizeiptr, Ptr a, BufferUsage)
bufferData t = makeStateVar (getBufferData t) (setBufferData t)

getBufferData :: BufferTarget -> IO (GLsizeiptr, Ptr a, BufferUsage)
getBufferData t = do
   s <- getBufferParameter t fromIntegral GetBufferSize
   p <- getBufferPointer t
   u <- getBufferParameter t unmarshalBufferUsage GetBufferUsage
   return (s, p, u)

setBufferData :: BufferTarget -> (GLsizeiptr, Ptr a, BufferUsage) -> IO ()
setBufferData t (s, p, u) =
   glBufferData (marshalBufferTarget t) s p (marshalBufferUsage u)

--------------------------------------------------------------------------------

data TransferDirection =
     ReadFromBuffer
   | WriteToBuffer
   deriving ( Eq, Ord, Show )

bufferSubData ::
   BufferTarget -> TransferDirection -> GLintptr -> GLsizeiptr -> Ptr a -> IO ()
bufferSubData t WriteToBuffer = glBufferSubData (marshalBufferTarget t)
bufferSubData t ReadFromBuffer = glGetBufferSubData (marshalBufferTarget t)

--------------------------------------------------------------------------------

data GetBufferPName =
     GetBufferSize
   | GetBufferUsage
   | GetBufferAccess
   | GetBufferMapped

marshalGetBufferPName :: GetBufferPName -> GLenum
marshalGetBufferPName x = case x of
   GetBufferSize -> gl_BUFFER_SIZE
   GetBufferUsage -> gl_BUFFER_USAGE
   GetBufferAccess -> gl_BUFFER_ACCESS
   GetBufferMapped -> gl_BUFFER_MAPPED

getBufferParameter :: BufferTarget -> (GLenum -> a) -> GetBufferPName -> IO a
getBufferParameter t f p = alloca $ \buf -> do
   glGetBufferParameteriv (marshalBufferTarget t)
                          (marshalGetBufferPName p) buf
   peek1 (f . fromIntegral) buf

--------------------------------------------------------------------------------

getBufferPointer :: BufferTarget -> IO (Ptr a)
getBufferPointer t = alloca $ \buf -> do
   glGetBufferPointerv (marshalBufferTarget t) gl_BUFFER_MAP_POINTER buf
   peek buf

--------------------------------------------------------------------------------

data MappingFailure =
     MappingFailed
   | UnmappingFailed
   deriving ( Eq, Ord, Show )

-- | Convenience function for an exception-safe combination of 'mapBuffer' and
-- 'unmapBuffer'.
withMappedBuffer :: BufferTarget -> BufferAccess -> (Ptr a -> IO b) -> (MappingFailure -> IO b) -> IO b
withMappedBuffer t a action err = do
   maybeBuf <- mapBuffer t a
   case maybeBuf  of
      Nothing -> err MappingFailed
      Just buf -> do (ret, ok) <- action buf `finallyRet` unmapBuffer t
                     if ok
                        then return ret
                        else err UnmappingFailed

mapBuffer :: BufferTarget -> BufferAccess -> IO (Maybe (Ptr a))
mapBuffer t = fmap (maybeNullPtr Nothing Just) . mapBuffer_ t

mapBuffer_ :: BufferTarget -> BufferAccess -> IO (Ptr a)
mapBuffer_ t = glMapBuffer (marshalBufferTarget t) . marshalBufferAccess

unmapBuffer :: BufferTarget -> IO Bool
unmapBuffer = fmap unmarshalGLboolean . glUnmapBuffer . marshalBufferTarget

bufferAccess :: BufferTarget -> GettableStateVar BufferAccess
bufferAccess t = makeGettableStateVar $
   getBufferParameter t unmarshalBufferAccess GetBufferAccess

bufferMapped :: BufferTarget -> GettableStateVar Bool
bufferMapped t = makeGettableStateVar $
   getBufferParameter t unmarshalGLboolean GetBufferMapped
