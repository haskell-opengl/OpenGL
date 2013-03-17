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
   module Graphics.Rendering.OpenGL.GL.ObjectName,

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
   bufferAccess, bufferMapped,

   BufferRangeAccessBit(..), Offset, Length,
   mapBufferRange, flushMappedBufferRange,

   -- * Indexed Buffer manipulation
   BufferIndex,
   RangeStartIndex, RangeSize,
   BufferRange,
   IndexedBufferTarget(..),
   bindBufferBase, bindBufferRange,
   indexedBufferStart, indexedBufferSize
) where

import Data.List(foldl1')
import Data.Bits((.|.))
import Data.Maybe

import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

import Graphics.Rendering.OpenGL.GL.StateVar
import Graphics.Rendering.OpenGL.GL.ObjectName
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
   | TransformFeedbackBuffer
   deriving ( Eq, Ord, Show )

marshalBufferTarget :: BufferTarget -> GLenum
marshalBufferTarget x = case x of
   ArrayBuffer -> gl_ARRAY_BUFFER
   CopyReadBuffer -> gl_COPY_READ_BUFFER
   CopyWriteBuffer -> gl_COPY_WRITE_BUFFER
   ElementArrayBuffer -> gl_ELEMENT_ARRAY_BUFFER
   PixelPackBuffer -> gl_PIXEL_PACK_BUFFER
   PixelUnpackBuffer -> gl_PIXEL_UNPACK_BUFFER
   TransformFeedbackBuffer -> gl_TRANSFORM_FEEDBACK_BUFFER

bufferTargetToGetPName :: BufferTarget -> PName1I
bufferTargetToGetPName x = case x of
   ArrayBuffer -> GetArrayBufferBinding
   ElementArrayBuffer -> GetElementArrayBufferBinding
   CopyReadBuffer -> GetCopyReadBuffer
   CopyWriteBuffer -> GetCopyWriteBuffer
   PixelPackBuffer -> GetPixelPackBufferBinding
   PixelUnpackBuffer -> GetPixelUnpackBufferBinding
   TransformFeedbackBuffer -> GetTransformFeedbackBufferBinding

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

--------------------------------------------------------------------------------

data BufferRangeAccessBit =
     ReadBit
   | WriteBit
   | InvalidateRangeBit
   | InvalidateBufferBit
   | FlushExplicitBit
   | UnsychronizedBit

type Offset = GLintptr
type Length = GLsizeiptr

marshalBufferRangeAccessBit :: BufferRangeAccessBit -> GLenum
marshalBufferRangeAccessBit x = case x of
    ReadBit -> gl_MAP_READ_BIT
    WriteBit -> gl_MAP_WRITE_BIT
    InvalidateRangeBit -> gl_MAP_INVALIDATE_RANGE_BIT
    InvalidateBufferBit -> gl_MAP_INVALIDATE_BUFFER_BIT
    FlushExplicitBit -> gl_MAP_FLUSH_EXPLICIT_BIT
    UnsychronizedBit -> gl_MAP_FLUSH_EXPLICIT_BIT

marshalToBitfield :: [BufferRangeAccessBit] -> GLenum
marshalToBitfield b = foldl1' (.|.)  $ map marshalBufferRangeAccessBit b

--------------------------------------------------------------------------------

mapBufferRange_ :: BufferTarget -> Offset -> Length ->
   [BufferRangeAccessBit] -> IO (Ptr a)
mapBufferRange_ t o l b = glMapBufferRange (marshalBufferTarget t) o l
    (fromIntegral $ marshalToBitfield b)

mapBufferRange :: BufferTarget -> Offset -> Length ->
   [BufferRangeAccessBit] -> IO (Maybe (Ptr a))
mapBufferRange t o l b = fmap (maybeNullPtr Nothing Just) $ mapBufferRange_ t o l b

flushMappedBufferRange :: BufferTarget -> Offset -> Length -> IO()
flushMappedBufferRange t = glFlushMappedBufferRange (marshalBufferTarget t)


--------------------------------------------------------------------------------
type BufferIndex = GLuint

type RangeStartIndex = GLintptr
type RangeSize = GLsizeiptr
type BufferRange = (BufferObject, RangeStartIndex, RangeSize)

data IndexedBufferTarget =
     IndexedTransformFeedBackbuffer
--marshaling
marshalIndexedBufferTarget :: IndexedBufferTarget -> IPName1I
marshalIndexedBufferTarget x = case x of
   IndexedTransformFeedBackbuffer -> GetTransformFeedbackBuffer

marshalIndexedBufferStart :: IndexedBufferTarget -> IPName1I
marshalIndexedBufferStart x = case x of
   IndexedTransformFeedBackbuffer -> GetTransformFeedbackBufferStart

marshalIndexedBufferSize :: IndexedBufferTarget -> IPName1I
marshalIndexedBufferSize x = case x of
   IndexedTransformFeedBackbuffer -> GetTransformFeedbackBufferSize

getIndexed :: Num a => IPName1I -> BufferIndex -> GettableStateVar a
getIndexed e i = makeGettableStateVar $ getInteger1i fromIntegral e i

--buffer
bindBufferBase :: IndexedBufferTarget -> BufferIndex -> StateVar (Maybe BufferObject)
bindBufferBase t i = makeStateVar (getIndexedBufferBinding t i) (setIndexedBufferBase t i)

setIndexedBufferBase :: IndexedBufferTarget -> BufferIndex -> Maybe BufferObject -> IO ()
setIndexedBufferBase t i buf=
   case marshalGetPName . marshalIndexedBufferTarget $ t of
      Nothing -> recordInvalidEnum
      Just t' ->
         glBindBufferBase t' i . bufferID . fromMaybe noBufferObject $ buf

getIndexedBufferBinding :: IndexedBufferTarget -> BufferIndex -> IO (Maybe BufferObject)
getIndexedBufferBinding t i = do
   buf <- getInteger1i (BufferObject . fromIntegral) (marshalIndexedBufferTarget t) i
   return $ if buf == noBufferObject then Nothing else Just buf

bindBufferRange :: IndexedBufferTarget -> BufferIndex -> StateVar (Maybe BufferRange)
bindBufferRange t i = makeStateVar (getIndexedBufferRange t i) (setIndexedBufferRange t i)

setIndexedBufferRange :: IndexedBufferTarget -> BufferIndex -> Maybe BufferRange -> IO ()
setIndexedBufferRange t i (Just (buf, start, range)) =
   case marshalGetPName . marshalIndexedBufferTarget $ t of
      Nothing -> recordInvalidEnum
      Just t' -> glBindBufferRange t' i (bufferID buf) start range
setIndexedBufferRange t i Nothing = setIndexedBufferBase t i Nothing

getIndexedBufferRange :: IndexedBufferTarget -> BufferIndex -> IO(Maybe BufferRange)
getIndexedBufferRange t i = do
  buf <- getInteger1i (BufferObject . fromIntegral) (marshalIndexedBufferTarget t) i
  if buf == noBufferObject
     then return Nothing
     else do
        start <- get $ indexedBufferStart t i
        size <- get $ indexedBufferSize t i
        return $ Just (buf, start, size)


indexedBufferStart :: IndexedBufferTarget -> BufferIndex -> GettableStateVar RangeStartIndex
indexedBufferStart = getIndexed . marshalIndexedBufferStart

indexedBufferSize :: IndexedBufferTarget -> BufferIndex -> GettableStateVar RangeSize
indexedBufferSize = getIndexed . marshalIndexedBufferSize
