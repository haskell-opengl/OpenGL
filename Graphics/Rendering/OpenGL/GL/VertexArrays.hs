--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.VertexArrays
-- Copyright   :  (c) Sven Panne 2002-2004
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
-- This module corresponds to section 2.8 (Vertex Arrays) of the OpenGL 1.5
-- specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.VertexArrays (
   -- * Describing Data for the Arrays
   NumComponents, DataType(..), Stride, VertexArrayDescriptor(..),

   -- * Specifying Data for the Arrays
   ClientArrayType(..), arrayPointer,
   InterleavedArrays(..), interleavedArrays,

   -- * Enabling Arrays
   clientState, clientActiveTexture,

   -- * Dereferencing and Rendering
   ArrayIndex, NumArrayIndices, NumIndexBlocks,
   arrayElement, drawArrays, multiDrawArrays, drawElements, multiDrawElements,
   drawRangeElements, maxElementsVertices, maxElementsIndices, lockArrays,
   primitiveRestartIndex
) where

import Control.Monad ( liftM )
import Data.List ( intersperse )
import Foreign.Ptr ( Ptr )
import Graphics.Rendering.OpenGL.GL.Capability (
   EnableCap(CapVertexArray,CapNormalArray,CapColorArray,CapIndexArray,
             CapTextureCoordArray,CapEdgeFlagArray,CapFogCoordArray,
             CapSecondaryColorArray,CapMatrixIndexArray,CapPrimitiveRestart),
   makeCapability )
import Graphics.Rendering.OpenGL.GL.BasicTypes (
   GLenum, GLint, GLuint, GLsizei, Capability(Enabled) )
import Graphics.Rendering.OpenGL.GL.DataType (
   DataType(..), marshalDataType, unmarshalDataType )
import Graphics.Rendering.OpenGL.GL.Extensions (
   FunPtr, unsafePerformIO, Invoker, getProcAddress )
import Graphics.Rendering.OpenGL.GL.QueryUtils (
   GetPName(GetVertexArraySize,GetVertexArrayType,GetVertexArrayStride,
            GetNormalArrayType,GetNormalArrayStride,GetColorArraySize,
            GetColorArrayType,GetColorArrayStride,GetSecondaryColorArraySize,
            GetSecondaryColorArrayType,GetSecondaryColorArrayStride,
            GetIndexArrayType,GetIndexArrayStride,
            GetFogCoordArrayType,GetFogCoordArrayStride,
            GetTextureCoordArraySize,GetTextureCoordArrayType,
            GetTextureCoordArrayStride,GetEdgeFlagArrayStride,
            GetMaxElementsVertices,GetMaxElementsIndices,
            GetClientActiveTexture,GetArrayElementLockFirst,
            GetArrayElementLockCount,GetPrimitiveRestartIndex),
   getInteger1, getEnum1, getSizei1,
   GetPointervPName(VertexArrayPointer,NormalArrayPointer,ColorArrayPointer,
                    SecondaryColorArrayPointer,IndexArrayPointer,
                    FogCoordArrayPointer,TextureCoordArrayPointer,
                    EdgeFlagArrayPointer),
   getPointer )
import Graphics.Rendering.OpenGL.GL.PrimitiveMode ( marshalPrimitiveMode )
import Graphics.Rendering.OpenGL.GL.BeginEnd ( PrimitiveMode )
import Graphics.Rendering.OpenGL.GL.StateVar (
   HasGetter(get),
   GettableStateVar, makeGettableStateVar, StateVar, makeStateVar )
import Graphics.Rendering.OpenGL.GL.VertexSpec (
   TextureUnit(TextureUnit) )

--------------------------------------------------------------------------------

#include "HsOpenGLExt.h"

--------------------------------------------------------------------------------

type NumComponents = GLint

type Stride = GLsizei

data VertexArrayDescriptor a =
   VertexArrayDescriptor NumComponents DataType Stride (Ptr a)
#ifdef __HADDOCK__
-- Help Haddock a bit, because it doesn't do any instance inference.
instance Eq (VertexArrayDescriptor a)
instance Ord (VertexArrayDescriptor a)
instance Show (VertexArrayDescriptor a)
#else
   deriving ( Eq, Ord, Show )
#endif

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
   VertexArray -> 0x8074
   NormalArray -> 0x8075
   ColorArray -> 0x8076
   IndexArray -> 0x8077
   TextureCoordArray -> 0x8078
   EdgeFlagArray -> 0x8079
   FogCoordArray -> 0x8457
   SecondaryColorArray -> 0x845e
   MatrixIndexArray -> 0x8844

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
   MatrixIndexArray -> error ("arrayPointer: MatrixIndexArray")

check :: (Eq a, Show a) => [a] -> a -> b -> b
check ns n val
   | n `elem` ns = val
   | otherwise =
      error ("arrayPointer: expected " ++ alts ++ " components, got " ++ show n)
      where alts = concat . intersperse " or " . map show $ ns

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

foreign import CALLCONV unsafe "glVertexPointer" glVertexPointer ::
   GLint -> GLenum -> GLsizei -> Ptr a -> IO ()

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
   check [3] n $ glNormalPointer (marshalDataType d) s p

foreign import CALLCONV unsafe "glNormalPointer" glNormalPointer ::
   GLenum -> GLsizei -> Ptr a -> IO ()

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
   check [3,4] n $ glColorPointer n (marshalDataType d) s p

foreign import CALLCONV unsafe "glColorPointer" glColorPointer ::
   GLint -> GLenum -> GLsizei -> Ptr a -> IO ()

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
   check [1] n $ glIndexPointer (marshalDataType d) s p

foreign import CALLCONV unsafe "glIndexPointer" glIndexPointer ::
   GLenum -> GLsizei -> Ptr a -> IO ()

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

foreign import CALLCONV unsafe "glTexCoordPointer" glTexCoordPointer ::
   GLint -> GLenum -> GLsizei -> Ptr a -> IO ()

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
   check [1] n $ check [UnsignedByte] d $ glEdgeFlagPointer s p

foreign import CALLCONV unsafe "glEdgeFlagPointer" glEdgeFlagPointer ::
   GLsizei -> Ptr a -> IO ()

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
   check [1] n $ glFogCoordPointerEXT (marshalDataType d) s p

EXTENSION_ENTRY("GL_EXT_fog_coord or OpenGL 1.4",glFogCoordPointerEXT,GLenum -> GLsizei -> Ptr a -> IO ())

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
   glSecondaryColorPointerEXT n (marshalDataType d) s p

EXTENSION_ENTRY("GL_EXT_secondary_color or OpenGL 1.4",glSecondaryColorPointerEXT,GLint -> GLenum -> GLsizei -> Ptr a -> IO ())

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
   V2f -> 0x2a20
   V3f -> 0x2a21
   C4ubV2f -> 0x2a22
   C4ubV3f -> 0x2a23
   C3fV3f -> 0x2a24
   N3fV3f -> 0x2a25
   C4fN3fV3f -> 0x2a26
   T2fV3f -> 0x2a27
   T4fV4f -> 0x2a28
   T2fC4ubV3f -> 0x2a29
   T2fC3fV3f -> 0x2a2a
   T2fN3fV3f -> 0x2a2b
   T2fC4fN3fV3f -> 0x2a2c
   T4fC4fN3fV4f -> 0x2a2d

--------------------------------------------------------------------------------

interleavedArrays :: InterleavedArrays -> Stride -> Ptr a -> IO ()
interleavedArrays = glInterleavedArrays . marshalInterleavedArrays

foreign import CALLCONV unsafe "glInterleavedArrays" glInterleavedArrays ::
   GLenum -> GLsizei -> Ptr a -> IO ()

--------------------------------------------------------------------------------

clientState :: ClientArrayType -> StateVar Capability
clientState arrayType =
   makeStateVar (getClientState arrayType) (setClientState arrayType)

getClientState :: ClientArrayType -> IO Capability
getClientState = get . makeCapability . clientArrayTypeToEnableCap

setClientState :: ClientArrayType -> Capability -> IO ()
setClientState arrayType val =
   (if val == Enabled then glEnableClientState else glDisableClientState)
      (marshalClientArrayType arrayType)

foreign import CALLCONV unsafe "glEnableClientState" glEnableClientState ::
   GLenum -> IO ()

foreign import CALLCONV unsafe "glDisableClientState" glDisableClientState ::
   GLenum -> IO ()

--------------------------------------------------------------------------------

clientActiveTexture :: StateVar TextureUnit
clientActiveTexture =
   makeStateVar (getEnum1 TextureUnit GetClientActiveTexture)
                (\(TextureUnit u) -> glClientActiveTextureARB u)

EXTENSION_ENTRY("GL_ARB_multitexture or OpenGL 1.3",glClientActiveTextureARB,GLenum -> IO ())

--------------------------------------------------------------------------------

type ArrayIndex = GLint

type NumArrayIndices = GLsizei

type NumIndexBlocks = GLsizei

--------------------------------------------------------------------------------

arrayElement :: ArrayIndex -> IO ()
arrayElement = glArrayElement

foreign import CALLCONV unsafe "glArrayElement" glArrayElement :: GLint -> IO ()

drawArrays :: PrimitiveMode -> ArrayIndex -> NumArrayIndices -> IO ()
drawArrays = glDrawArrays . marshalPrimitiveMode

foreign import CALLCONV unsafe "glDrawArrays" glDrawArrays ::
   GLenum -> GLint -> GLsizei -> IO ()

multiDrawArrays ::
      PrimitiveMode -> Ptr ArrayIndex -> Ptr NumArrayIndices -> NumIndexBlocks
   -> IO ()
multiDrawArrays = glMultiDrawArraysEXT . marshalPrimitiveMode

EXTENSION_ENTRY("GL_EXT_multi_draw_arrays or OpenGL 1.4",glMultiDrawArraysEXT,GLenum -> Ptr ArrayIndex -> Ptr GLsizei -> GLsizei -> IO ())

drawElements :: PrimitiveMode -> NumArrayIndices -> DataType -> Ptr a -> IO ()
drawElements m c = glDrawElements (marshalPrimitiveMode m) c . marshalDataType

foreign import CALLCONV unsafe "glDrawElements" glDrawElements ::
   GLenum -> GLsizei -> GLenum -> Ptr a -> IO ()

multiDrawElements ::
      PrimitiveMode -> Ptr NumArrayIndices -> DataType -> Ptr (Ptr a)
   -> NumIndexBlocks -> IO ()
multiDrawElements m c =
   glMultiDrawElementsEXT (marshalPrimitiveMode m) c . marshalDataType

EXTENSION_ENTRY("GL_EXT_multi_draw_arrays or OpenGL 1.4",glMultiDrawElementsEXT,GLenum -> Ptr GLsizei -> GLenum -> Ptr (Ptr a) -> GLsizei -> IO ())

drawRangeElements ::
      PrimitiveMode -> (ArrayIndex, ArrayIndex) -> NumArrayIndices -> DataType
   -> Ptr a -> IO ()
drawRangeElements m (s, e) c =
   glDrawRangeElementsEXT (marshalPrimitiveMode m) (fromIntegral s)
                          (fromIntegral e) c . marshalDataType

EXTENSION_ENTRY("GL_EXT_draw_range_elements or OpenGL 1.2",glDrawRangeElementsEXT,GLenum -> GLuint -> GLuint -> GLsizei -> GLenum -> Ptr a -> IO ())

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

EXTENSION_ENTRY("GL_EXT_compiled_vertex_array",glLockArraysEXT,GLint -> GLsizei -> IO ())
EXTENSION_ENTRY("GL_EXT_compiled_vertex_array",glUnlockArraysEXT,IO ())

--------------------------------------------------------------------------------

-- We almost could use makeStateVarMaybe below, but, alas, this is client state.

primitiveRestartIndex :: StateVar (Maybe ArrayIndex)
primitiveRestartIndex =
   makeStateVar getPrimitiverestartIndex setPrimitiverestartIndex

getPrimitiverestartIndex :: IO (Maybe ArrayIndex)
getPrimitiverestartIndex = do
   state <- get (makeCapability CapPrimitiveRestart)
   if state == Enabled
      then liftM Just $ getInteger1 fromIntegral GetPrimitiveRestartIndex
      else return Nothing

setPrimitiverestartIndex :: Maybe ArrayIndex -> IO ()
setPrimitiverestartIndex maybeIdx = case maybeIdx of
   Nothing  -> glDisableClientState primitiveRestartNV
   Just idx -> do glEnableClientState primitiveRestartNV
                  glPrimitiveRestartIndexNV (fromIntegral idx)
   where primitiveRestartNV = 0x8558   -- ToDo: HACK!

EXTENSION_ENTRY("GL_NV_primitive_restart",glPrimitiveRestartIndexNV,GLuint -> IO ())
