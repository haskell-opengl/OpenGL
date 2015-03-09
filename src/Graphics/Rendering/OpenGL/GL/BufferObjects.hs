--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.BufferObjects
-- Copyright   :  (c) Sven Panne 2002-2013
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to section 2.9 (Buffer Objects) of the OpenGL 2.1
-- specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.BufferObjects (
   -- * Buffer Objects
   BufferObject,

   -- * Binding Buffer Objects
   BufferTarget(..), bindBuffer, arrayBufferBinding,
   vertexAttribArrayBufferBinding,

   -- * Handling Buffer Data
   BufferUsage(..), bufferData, TransferDirection(..), bufferSubData,

   -- * Mapping Buffer Objects
   BufferAccess(..), MappingFailure(..), withMappedBuffer,
   mapBuffer, unmapBuffer,
   bufferAccess, bufferMapped,

   MapBufferUsage(..), Offset, Length,
   mapBufferRange, flushMappedBufferRange,

   -- * Indexed Buffer manipulation
   BufferIndex,
   RangeStartIndex, RangeSize,
   BufferRange,
   IndexedBufferTarget(..),
   bindBufferBase, bindBufferRange,
   indexedBufferStart, indexedBufferSize
) where

import Control.Monad.IO.Class
import Data.Maybe ( fromMaybe )
import Data.ObjectName
import Data.StateVar
import Foreign.Marshal.Array ( allocaArray, peekArray, withArrayLen )
import Foreign.Marshal.Utils ( with )
import Foreign.Ptr ( Ptr, nullPtr )
import Graphics.Rendering.OpenGL.GL.DebugOutput
import Graphics.Rendering.OpenGL.GL.Exception
import Graphics.Rendering.OpenGL.GL.GLboolean
import Graphics.Rendering.OpenGL.GL.PeekPoke
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.GL.VertexArrays
import Graphics.Rendering.OpenGL.GLU.ErrorsInternal
import Graphics.Rendering.OpenGL.Raw

--------------------------------------------------------------------------------

newtype BufferObject = BufferObject { bufferID :: GLuint }
   deriving ( Eq, Ord, Show )

--------------------------------------------------------------------------------

instance ObjectName BufferObject where
   isObjectName = liftIO . fmap unmarshalGLboolean . glIsBuffer . bufferID

   deleteObjectNames bufferObjects =
      liftIO . withArrayLen (map bufferID bufferObjects) $
         glDeleteBuffers . fromIntegral

instance GeneratableObjectName BufferObject where
   genObjectNames n =
      liftIO . allocaArray n $ \buf -> do
        glGenBuffers (fromIntegral n) buf
        fmap (map BufferObject) $ peekArray n buf

instance CanBeLabeled BufferObject where
   objectLabel = objectNameLabel gl_BUFFER . bufferID

--------------------------------------------------------------------------------

data BufferTarget =
     ArrayBuffer
   | AtomicCounterBuffer
   | CopyReadBuffer
   | CopyWriteBuffer
   | DispatchIndirectBuffer
   | DrawIndirectBuffer
   | ElementArrayBuffer
   | PixelPackBuffer
   | PixelUnpackBuffer
   | QueryBuffer
   | ShaderStorageBuffer
   | TextureBuffer
   | TransformFeedbackBuffer
   | UniformBuffer
   deriving ( Eq, Ord, Show )

marshalBufferTarget :: BufferTarget -> GLenum
marshalBufferTarget x = case x of
   ArrayBuffer -> gl_ARRAY_BUFFER
   AtomicCounterBuffer -> gl_ATOMIC_COUNTER_BUFFER
   CopyReadBuffer -> gl_COPY_READ_BUFFER
   CopyWriteBuffer -> gl_COPY_WRITE_BUFFER
   DispatchIndirectBuffer -> gl_DISPATCH_INDIRECT_BUFFER
   DrawIndirectBuffer -> gl_DRAW_INDIRECT_BUFFER
   ElementArrayBuffer -> gl_ELEMENT_ARRAY_BUFFER
   PixelPackBuffer -> gl_PIXEL_PACK_BUFFER
   PixelUnpackBuffer -> gl_PIXEL_UNPACK_BUFFER
   QueryBuffer -> gl_QUERY_BUFFER
   ShaderStorageBuffer -> gl_SHADER_STORAGE_BUFFER
   TextureBuffer -> gl_TEXTURE_BUFFER
   TransformFeedbackBuffer -> gl_TRANSFORM_FEEDBACK_BUFFER
   UniformBuffer -> gl_UNIFORM_BUFFER

bufferTargetToGetPName :: BufferTarget -> PName1I
bufferTargetToGetPName x = case x of
   ArrayBuffer -> GetArrayBufferBinding
   AtomicCounterBuffer -> GetAtomicCounterBufferBinding
   CopyReadBuffer -> GetCopyReadBufferBinding
   CopyWriteBuffer -> GetCopyWriteBufferBinding
   DispatchIndirectBuffer -> GetDispatchIndirectBufferBinding
   DrawIndirectBuffer -> GetDrawIndirectBufferBinding
   ElementArrayBuffer -> GetElementArrayBufferBinding
   PixelPackBuffer -> GetPixelPackBufferBinding
   PixelUnpackBuffer -> GetPixelUnpackBufferBinding
   QueryBuffer -> GetQueryBufferBinding
   ShaderStorageBuffer -> GetShaderStorageBufferBinding
   TextureBuffer -> GetTextureBindingBuffer
   TransformFeedbackBuffer -> GetTransformFeedbackBufferBinding
   UniformBuffer -> GetUniformBufferBinding

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

bufferQuery :: (a -> PName1I) -> a -> IO (Maybe BufferObject)
bufferQuery func t = do
   buf <- getInteger1 (BufferObject . fromIntegral) (func t)
   return $ if buf == noBufferObject then Nothing else Just buf

noBufferObject :: BufferObject
noBufferObject = BufferObject 0

setBindBuffer :: BufferTarget -> Maybe BufferObject -> IO ()
setBindBuffer t =
   glBindBuffer (marshalBufferTarget t) . bufferID . fromMaybe noBufferObject

clientArrayTypeToGetPName :: ClientArrayType -> PName1I
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
getBufferParameter t f p = with 0 $ \buf -> do
   glGetBufferParameteriv (marshalBufferTarget t)
                          (marshalGetBufferPName p) buf
   peek1 (f . fromIntegral) buf

--------------------------------------------------------------------------------

getBufferPointer :: BufferTarget -> IO (Ptr a)
getBufferPointer t = with nullPtr $ \buf -> do
   glGetBufferPointerv (marshalBufferTarget t) gl_BUFFER_MAP_POINTER buf
   peek1 id buf

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

--------------------------------------------------------------------------------

data MapBufferUsage =
     Read
   | Write
   | InvalidateRange
   | InvalidateBuffer
   | FlushExplicit
   | Unsychronized
   deriving ( Eq, Ord, Show )

type Offset = GLintptr
type Length = GLsizeiptr

marshalMapBufferUsage :: MapBufferUsage -> GLbitfield
marshalMapBufferUsage x = case x of
    Read -> gl_MAP_READ_BIT
    Write -> gl_MAP_WRITE_BIT
    InvalidateRange -> gl_MAP_INVALIDATE_RANGE_BIT
    InvalidateBuffer -> gl_MAP_INVALIDATE_BUFFER_BIT
    FlushExplicit -> gl_MAP_FLUSH_EXPLICIT_BIT
    Unsychronized -> gl_MAP_FLUSH_EXPLICIT_BIT

--------------------------------------------------------------------------------

mapBufferRange_ ::
   BufferTarget -> Offset -> Length -> [MapBufferUsage] -> IO (Ptr a)
mapBufferRange_ t o l b = glMapBufferRange (marshalBufferTarget t) o l
   (sum (map marshalMapBufferUsage b))

mapBufferRange ::
   BufferTarget -> Offset -> Length -> [MapBufferUsage] -> IO (Maybe (Ptr a))
mapBufferRange t o l b =
   fmap (maybeNullPtr Nothing Just) $ mapBufferRange_ t o l b

flushMappedBufferRange :: BufferTarget -> Offset -> Length -> IO ()
flushMappedBufferRange t = glFlushMappedBufferRange (marshalBufferTarget t)

--------------------------------------------------------------------------------

type BufferIndex = GLuint

type RangeStartIndex = GLintptr
type RangeSize = GLsizeiptr
type BufferRange = (BufferObject, RangeStartIndex, RangeSize)

data IndexedBufferTarget =
     IndexedAtomicCounterBuffer
   | IndexedShaderStorageBuffer
   | IndexedTransformFeedbackBuffer
   | IndexedUniformBuffer
   deriving ( Eq, Ord, Show )

marshalIndexedBufferTarget :: IndexedBufferTarget -> IPName1I
marshalIndexedBufferTarget x = case x of
   IndexedAtomicCounterBuffer -> GetAtomicCounterBuffer
   IndexedShaderStorageBuffer -> GetShaderStorageBuffer
   IndexedTransformFeedbackBuffer -> GetTransformFeedbackBuffer
   IndexedUniformBuffer -> GetUniformBuffer

bindBufferBase :: IndexedBufferTarget -> BufferIndex -> StateVar (Maybe BufferObject)
bindBufferBase t i = makeStateVar (getIndexedBufferBinding t i) (setIndexedBufferBase t i)

getIndexedBufferBinding :: IndexedBufferTarget -> BufferIndex -> IO (Maybe BufferObject)
getIndexedBufferBinding t i = do
   buf <- getInteger1i (BufferObject . fromIntegral) (marshalIndexedBufferTarget t) i
   return $ if buf == noBufferObject then Nothing else Just buf

setIndexedBufferBase :: IndexedBufferTarget -> BufferIndex -> Maybe BufferObject -> IO ()
setIndexedBufferBase t i buf =
   case marshalGetPName . marshalIndexedBufferTarget $ t of
      Nothing -> recordInvalidEnum
      Just t' -> glBindBufferBase t' i . bufferID . fromMaybe noBufferObject $ buf

bindBufferRange :: IndexedBufferTarget -> BufferIndex -> StateVar (Maybe BufferRange)
bindBufferRange t i = makeStateVar (getIndexedBufferRange t i) (setIndexedBufferRange t i)

getIndexedBufferRange :: IndexedBufferTarget -> BufferIndex -> IO (Maybe BufferRange)
getIndexedBufferRange t i = do
  buf <- getInteger1i (BufferObject . fromIntegral) (marshalIndexedBufferTarget t) i
  if buf == noBufferObject
     then return Nothing
     else do start <- get $ indexedBufferStart t i
             size <- get $ indexedBufferSize t i
             return $ Just (buf, start, size)

setIndexedBufferRange :: IndexedBufferTarget -> BufferIndex -> Maybe BufferRange -> IO ()
setIndexedBufferRange t i br =
   case marshalGetPName . marshalIndexedBufferTarget $ t of
      Nothing -> recordInvalidEnum
      Just t' -> glBindBufferRange t' i (bufferID buf) start range
   where (buf, start, range) = fromMaybe (noBufferObject, 0, 0) br

getIndexed :: Num a => IPName1I -> BufferIndex -> GettableStateVar a
getIndexed e i = makeGettableStateVar $ getInteger641i fromIntegral e i

marshalIndexedBufferStart :: IndexedBufferTarget -> IPName1I
marshalIndexedBufferStart x = case x of
   IndexedAtomicCounterBuffer -> GetAtomicCounterBufferStart
   IndexedShaderStorageBuffer -> GetShaderStorageBufferStart
   IndexedTransformFeedbackBuffer -> GetTransformFeedbackBufferStart
   IndexedUniformBuffer -> GetUniformBufferStart

indexedBufferStart :: IndexedBufferTarget -> BufferIndex -> GettableStateVar RangeStartIndex
indexedBufferStart = getIndexed . marshalIndexedBufferStart

marshalIndexedBufferSize :: IndexedBufferTarget -> IPName1I
marshalIndexedBufferSize x = case x of
   IndexedAtomicCounterBuffer -> GetAtomicCounterBufferSize
   IndexedShaderStorageBuffer -> GetShaderStorageBufferSize
   IndexedTransformFeedbackBuffer -> GetTransformFeedbackBufferSize
   IndexedUniformBuffer -> GetUniformBufferSize

indexedBufferSize :: IndexedBufferTarget -> BufferIndex -> GettableStateVar RangeSize
indexedBufferSize = getIndexed . marshalIndexedBufferSize
