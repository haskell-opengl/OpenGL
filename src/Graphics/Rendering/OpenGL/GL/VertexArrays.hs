--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.VertexArrays
-- Copyright   :  (c) Sven Panne 2002-2013
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to section 2.8 (Vertex Arrays) of the OpenGL 2.1
-- specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.VertexArrays (
   -- * Describing Data for the Arrays
   NumComponents, DataType(..), Stride, VertexArrayDescriptor(..),

   -- * Specifying Data for the Arrays
   Capability(..),
   ClientArrayType(..), arrayPointer,
   InterleavedArrays(..), interleavedArrays,

   -- * Enabling Arrays
   clientState, clientActiveTexture,

   -- * Dereferencing and Rendering
   ArrayIndex, NumArrayIndices, NumIndexBlocks,
   arrayElement, drawArrays, multiDrawArrays, drawElements, multiDrawElements,
   drawRangeElements, maxElementsVertices, maxElementsIndices, lockArrays,
   primitiveRestartIndex, primitiveRestartIndexNV,

   -- * Generic Vertex Attribute Arrays
   vertexAttribPointer, vertexAttribArray,
) where

import Data.StateVar
import Foreign.Ptr ( Ptr, nullPtr )
import Graphics.Rendering.OpenGL.GL.Capability
import Graphics.Rendering.OpenGL.GL.DataType
import Graphics.Rendering.OpenGL.GL.GLboolean
import Graphics.Rendering.OpenGL.GL.PrimitiveMode
import Graphics.Rendering.OpenGL.GL.PrimitiveModeInternal
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.GL.Texturing.TextureUnit
import Graphics.Rendering.OpenGL.GL.VertexSpec
import Graphics.Rendering.OpenGL.GLU.ErrorsInternal
import Graphics.Rendering.OpenGL.Raw

--------------------------------------------------------------------------------

type NumComponents = GLint

type Stride = GLsizei

data VertexArrayDescriptor a =
   VertexArrayDescriptor !NumComponents !DataType !Stride !(Ptr a)
   deriving ( Eq, Ord, Show )

noVertexArrayDescriptor :: VertexArrayDescriptor a
noVertexArrayDescriptor = VertexArrayDescriptor 0 Byte 0 nullPtr

--------------------------------------------------------------------------------

data ClientArrayType =
     VertexArray
   | NormalArray
   | ColorArray
   | IndexArray
   | TextureCoordArray
   | EdgeFlagArray
   | FogCoordArray
   | SecondaryColorArray
   | MatrixIndexArray
   deriving ( Eq, Ord, Show )

marshalClientArrayType :: ClientArrayType -> GLenum
marshalClientArrayType x = case x of
   VertexArray -> gl_VERTEX_ARRAY
   NormalArray -> gl_NORMAL_ARRAY
   ColorArray -> gl_COLOR_ARRAY
   IndexArray -> gl_INDEX_ARRAY
   TextureCoordArray -> gl_TEXTURE_COORD_ARRAY
   EdgeFlagArray -> gl_EDGE_FLAG_ARRAY
   FogCoordArray -> gl_FOG_COORD_ARRAY
   SecondaryColorArray -> gl_SECONDARY_COLOR_ARRAY
   MatrixIndexArray -> gl_MATRIX_INDEX_ARRAY_ARB

-- Hmmm...
clientArrayTypeToEnableCap :: ClientArrayType -> EnableCap
clientArrayTypeToEnableCap x = case x of
   VertexArray -> CapVertexArray
   NormalArray -> CapNormalArray
   ColorArray -> CapColorArray
   IndexArray -> CapIndexArray
   TextureCoordArray -> CapTextureCoordArray
   EdgeFlagArray -> CapEdgeFlagArray
   FogCoordArray -> CapFogCoordArray
   SecondaryColorArray -> CapSecondaryColorArray
   MatrixIndexArray -> CapMatrixIndexArray

--------------------------------------------------------------------------------

arrayPointer :: ClientArrayType -> StateVar (VertexArrayDescriptor a)
arrayPointer t = case t of
   VertexArray -> vertexPointer
   NormalArray -> normalPointer
   ColorArray -> colorPointer
   IndexArray -> indexPointer
   TextureCoordArray -> texCoordPointer
   EdgeFlagArray -> edgeFlagPointer
   FogCoordArray -> fogCoordPointer
   SecondaryColorArray -> secondaryColorPointer
   MatrixIndexArray ->
      makeStateVar
        (do recordInvalidEnum ; return noVertexArrayDescriptor)
        (const recordInvalidEnum)

check :: Bool -> IO () -> IO ()
check flag val = if flag then val else recordInvalidValue

--------------------------------------------------------------------------------

vertexPointer :: StateVar (VertexArrayDescriptor a)
vertexPointer = makeStateVar getVertexPointer setVertexPointer

getVertexPointer :: IO (VertexArrayDescriptor a)
getVertexPointer = do
   n <- getInteger1 id GetVertexArraySize
   d <- getEnum1 unmarshalDataType GetVertexArrayType
   s <- getInteger1 fromIntegral GetVertexArrayStride
   p <- getPointer VertexArrayPointer
   return $ VertexArrayDescriptor n d s p

setVertexPointer :: VertexArrayDescriptor a -> IO ()
setVertexPointer (VertexArrayDescriptor n d s p) =
   glVertexPointer n (marshalDataType d) s p

--------------------------------------------------------------------------------

normalPointer :: StateVar (VertexArrayDescriptor a)
normalPointer = makeStateVar getNormalPointer setNormalPointer

getNormalPointer :: IO (VertexArrayDescriptor a)
getNormalPointer = do
   d <- getEnum1 unmarshalDataType GetNormalArrayType
   s <- getInteger1 fromIntegral GetNormalArrayStride
   p <- getPointer NormalArrayPointer
   return $ VertexArrayDescriptor 3 d s p

setNormalPointer :: VertexArrayDescriptor a -> IO ()
setNormalPointer (VertexArrayDescriptor n d s p) =
   check (n == 3) $ glNormalPointer (marshalDataType d) s p

--------------------------------------------------------------------------------

colorPointer :: StateVar (VertexArrayDescriptor a)
colorPointer = makeStateVar getColorPointer setColorPointer

getColorPointer :: IO (VertexArrayDescriptor a)
getColorPointer = do
   n <- getInteger1 id GetColorArraySize
   d <- getEnum1 unmarshalDataType GetColorArrayType
   s <- getInteger1 fromIntegral GetColorArrayStride
   p <- getPointer ColorArrayPointer
   return $ VertexArrayDescriptor n d s p

setColorPointer :: VertexArrayDescriptor a -> IO ()
setColorPointer (VertexArrayDescriptor n d s p) =
   check (n == 3 || n == 4) $ glColorPointer n (marshalDataType d) s p

--------------------------------------------------------------------------------

indexPointer :: StateVar (VertexArrayDescriptor a)
indexPointer = makeStateVar getIndexPointer setIndexPointer

getIndexPointer :: IO (VertexArrayDescriptor a)
getIndexPointer = do
   d <- getEnum1 unmarshalDataType GetIndexArrayType
   s <- getInteger1 fromIntegral GetIndexArrayStride
   p <- getPointer IndexArrayPointer
   return $ VertexArrayDescriptor 1 d s p

setIndexPointer :: VertexArrayDescriptor a -> IO ()
setIndexPointer (VertexArrayDescriptor n d s p) =
   check (n == 1) $ glIndexPointer (marshalDataType d) s p

--------------------------------------------------------------------------------

texCoordPointer :: StateVar (VertexArrayDescriptor a)
texCoordPointer = makeStateVar getTexCoordPointer setTexCoordPointer

getTexCoordPointer :: IO (VertexArrayDescriptor a)
getTexCoordPointer = do
   n <- getInteger1 id GetTextureCoordArraySize
   d <- getEnum1 unmarshalDataType GetTextureCoordArrayType
   s <- getInteger1 fromIntegral GetTextureCoordArrayStride
   p <- getPointer TextureCoordArrayPointer
   return $ VertexArrayDescriptor n d s p

setTexCoordPointer :: VertexArrayDescriptor a -> IO ()
setTexCoordPointer (VertexArrayDescriptor n d s p) =
   glTexCoordPointer n (marshalDataType d) s p

--------------------------------------------------------------------------------

edgeFlagPointer :: StateVar (VertexArrayDescriptor a)
edgeFlagPointer = makeStateVar getEdgeFlagPointer setEdgeFlagPointer

getEdgeFlagPointer :: IO (VertexArrayDescriptor a)
getEdgeFlagPointer = do
   s <- getInteger1 fromIntegral GetEdgeFlagArrayStride
   p <- getPointer EdgeFlagArrayPointer
   return $ VertexArrayDescriptor 1 UnsignedByte s p

setEdgeFlagPointer :: VertexArrayDescriptor a -> IO ()
setEdgeFlagPointer (VertexArrayDescriptor n d s p) =
   check (n == 1 && d == UnsignedByte) $ glEdgeFlagPointer s p

--------------------------------------------------------------------------------

fogCoordPointer :: StateVar (VertexArrayDescriptor a)
fogCoordPointer = makeStateVar getFogCoordPointer setFogCoordPointer

getFogCoordPointer :: IO (VertexArrayDescriptor a)
getFogCoordPointer = do
   d <- getEnum1 unmarshalDataType GetFogCoordArrayType
   s <- getInteger1 fromIntegral GetFogCoordArrayStride
   p <- getPointer FogCoordArrayPointer
   return $ VertexArrayDescriptor 1 d s p

setFogCoordPointer :: VertexArrayDescriptor a -> IO ()
setFogCoordPointer (VertexArrayDescriptor n d s p) =
   check (n == 1) $ glFogCoordPointer (marshalDataType d) s p

--------------------------------------------------------------------------------

secondaryColorPointer :: StateVar (VertexArrayDescriptor a)
secondaryColorPointer =
   makeStateVar getSecondaryColorPointer setSecondaryColorPointer

getSecondaryColorPointer :: IO (VertexArrayDescriptor a)
getSecondaryColorPointer = do
   n <- getInteger1 id GetSecondaryColorArraySize
   d <- getEnum1 unmarshalDataType GetSecondaryColorArrayType
   s <- getInteger1 fromIntegral GetSecondaryColorArrayStride
   p <- getPointer SecondaryColorArrayPointer
   return $ VertexArrayDescriptor n d s p

setSecondaryColorPointer :: (VertexArrayDescriptor a) -> IO ()
setSecondaryColorPointer (VertexArrayDescriptor n d s p) =
   glSecondaryColorPointer n (marshalDataType d) s p

--------------------------------------------------------------------------------

data InterleavedArrays =
     V2f
   | V3f
   | C4ubV2f
   | C4ubV3f
   | C3fV3f
   | N3fV3f
   | C4fN3fV3f
   | T2fV3f
   | T4fV4f
   | T2fC4ubV3f
   | T2fC3fV3f
   | T2fN3fV3f
   | T2fC4fN3fV3f
   | T4fC4fN3fV4f
   deriving ( Eq, Ord, Show )

marshalInterleavedArrays :: InterleavedArrays -> GLenum
marshalInterleavedArrays x = case x of
   V2f -> gl_V2F
   V3f -> gl_V3F
   C4ubV2f -> gl_C4UB_V2F
   C4ubV3f -> gl_C4UB_V3F
   C3fV3f -> gl_C3F_V3F
   N3fV3f -> gl_N3F_V3F
   C4fN3fV3f -> gl_C4F_N3F_V3F
   T2fV3f -> gl_T2F_V3F
   T4fV4f -> gl_T4F_V4F
   T2fC4ubV3f -> gl_T2F_C4UB_V3F
   T2fC3fV3f -> gl_T2F_C3F_V3F
   T2fN3fV3f -> gl_T2F_N3F_V3F
   T2fC4fN3fV3f -> gl_T2F_C4F_N3F_V3F
   T4fC4fN3fV4f -> gl_T4F_C4F_N3F_V4F

--------------------------------------------------------------------------------

interleavedArrays :: InterleavedArrays -> Stride -> Ptr a -> IO ()
interleavedArrays = glInterleavedArrays . marshalInterleavedArrays

--------------------------------------------------------------------------------

clientState :: ClientArrayType -> StateVar Capability
clientState arrayType =
   makeStateVar (getClientState arrayType) (setClientState arrayType)

getClientState :: ClientArrayType -> IO Capability
getClientState arrayType = get . makeCapability . clientArrayTypeToEnableCap $ arrayType

setClientState :: ClientArrayType -> Capability -> IO ()
setClientState arrayType val =
   (if val == Enabled then glEnableClientState else glDisableClientState)
      (marshalClientArrayType arrayType)

--------------------------------------------------------------------------------

clientActiveTexture :: StateVar TextureUnit
clientActiveTexture =
   makeStateVar (getEnum1 unmarshalTextureUnit GetClientActiveTexture)
                (glClientActiveTexture . marshalTextureUnit)

--------------------------------------------------------------------------------

type ArrayIndex = GLint

type NumArrayIndices = GLsizei

type NumIndexBlocks = GLsizei

--------------------------------------------------------------------------------

arrayElement :: ArrayIndex -> IO ()
arrayElement = glArrayElement

drawArrays :: PrimitiveMode -> ArrayIndex -> NumArrayIndices -> IO ()
drawArrays = glDrawArrays . marshalPrimitiveMode

multiDrawArrays ::
      PrimitiveMode -> Ptr ArrayIndex -> Ptr NumArrayIndices -> NumIndexBlocks
   -> IO ()
multiDrawArrays = glMultiDrawArrays . marshalPrimitiveMode

drawElements :: PrimitiveMode -> NumArrayIndices -> DataType -> Ptr a -> IO ()
drawElements m c = glDrawElements (marshalPrimitiveMode m) c . marshalDataType

multiDrawElements ::
      PrimitiveMode -> Ptr NumArrayIndices -> DataType -> Ptr (Ptr a)
   -> NumIndexBlocks -> IO ()
multiDrawElements m c =
   glMultiDrawElements (marshalPrimitiveMode m) c . marshalDataType

drawRangeElements ::
      PrimitiveMode -> (ArrayIndex, ArrayIndex) -> NumArrayIndices -> DataType
   -> Ptr a -> IO ()
drawRangeElements m (s, e) c =
   glDrawRangeElements (marshalPrimitiveMode m) (fromIntegral s)
                       (fromIntegral e) c . marshalDataType

maxElementsVertices :: GettableStateVar NumArrayIndices
maxElementsVertices = makeGettableStateVar (getSizei1 id GetMaxElementsVertices)

maxElementsIndices :: GettableStateVar NumArrayIndices
maxElementsIndices = makeGettableStateVar (getSizei1 id GetMaxElementsIndices)

--------------------------------------------------------------------------------

lockArrays :: StateVar (Maybe (ArrayIndex, NumArrayIndices))
lockArrays = makeStateVar getLockArrays setLockArrays

getLockArrays :: IO (Maybe (ArrayIndex, NumArrayIndices))
getLockArrays = do
   count <- getInteger1 fromIntegral GetArrayElementLockCount
   if count > 0
      then do first <- getInteger1 id GetArrayElementLockFirst
              return $ Just (first, count)
      else return Nothing

setLockArrays :: Maybe (ArrayIndex, NumArrayIndices) -> IO ()
setLockArrays = maybe glUnlockArraysEXT (uncurry glLockArraysEXT)

--------------------------------------------------------------------------------

primitiveRestartIndex :: StateVar (Maybe ArrayIndex)
primitiveRestartIndex =
   makeStateVarMaybe
      (return CapPrimitiveRestart)
      (getInteger1 id GetPrimitiveRestartIndex)
      (glPrimitiveRestartIndex . fromIntegral)

--------------------------------------------------------------------------------

-- We almost could use makeStateVarMaybe below, but, alas, this is client state.

primitiveRestartIndexNV :: StateVar (Maybe ArrayIndex)
primitiveRestartIndexNV =
   makeStateVar getPrimitiveRestartIndexNV setPrimitiveRestartIndexNV

getPrimitiveRestartIndexNV :: IO (Maybe ArrayIndex)
getPrimitiveRestartIndexNV = do
   on <- getBoolean1 unmarshalGLboolean GetPrimitiveRestartNV
   if on
      then fmap Just $ getInteger1 fromIntegral GetPrimitiveRestartIndexNV
      else return Nothing

setPrimitiveRestartIndexNV :: Maybe ArrayIndex -> IO ()
setPrimitiveRestartIndexNV maybeIdx = case maybeIdx of
   Nothing  -> glDisableClientState gl_PRIMITIVE_RESTART_NV
   Just idx -> do glEnableClientState gl_PRIMITIVE_RESTART_NV
                  glPrimitiveRestartIndexNV (fromIntegral idx)

--------------------------------------------------------------------------------

vertexAttribPointer :: AttribLocation -> StateVar (IntegerHandling, VertexArrayDescriptor a)
vertexAttribPointer location =
   makeStateVar (getVertexAttribPointer_ location) (setVertexAttribPointer location)

getVertexAttribPointer_ :: AttribLocation -> IO (IntegerHandling, VertexArrayDescriptor a)
getVertexAttribPointer_ location = do
   i <- getVertexAttribBoolean1 unmarshalGLboolean location GetVertexAttribArrayInteger
   h <- if i
           then return KeepIntegral
           else do f <- getVertexAttribBoolean1 unmarshalGLboolean location GetVertexAttribArrayNormalized
                   return $ if f then ToNormalizedFloat else ToFloat
   n <- getVertexAttribInteger1 id location GetVertexAttribArraySize
   d <- getVertexAttribEnum1 unmarshalDataType location GetVertexAttribArrayType
   s <- getVertexAttribInteger1 fromIntegral location GetVertexAttribArrayStride
   p <- getVertexAttribPointer location VertexAttribArrayPointer
   return (h, VertexArrayDescriptor n d s p)

setVertexAttribPointer :: AttribLocation -> (IntegerHandling, VertexArrayDescriptor a) -> IO ()
setVertexAttribPointer (AttribLocation location) (h, VertexArrayDescriptor n d s p) = case h of
   ToFloat -> glVertexAttribPointer location n md (marshalGLboolean False) s p
   ToNormalizedFloat -> glVertexAttribPointer location n md (marshalGLboolean True) s p
   KeepIntegral -> glVertexAttribIPointer location n md s p
  where md = marshalDataType d

--------------------------------------------------------------------------------

vertexAttribArray :: AttribLocation -> StateVar Capability
vertexAttribArray location =
   makeStateVar (getVertexAttribArray location) (flip setVertexAttribArray location)

getVertexAttribArray :: AttribLocation -> IO Capability
getVertexAttribArray location =
   getVertexAttribBoolean1 unmarshalCapability location GetVertexAttribArrayEnabled

setVertexAttribArray :: Capability -> AttribLocation -> IO ()
setVertexAttribArray Disabled (AttribLocation location) = glDisableVertexAttribArray location
setVertexAttribArray Enabled (AttribLocation location) = glEnableVertexAttribArray location
