--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.PixelRectangles
-- Copyright   :  (c) Sven Panne 2002-2004
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
-- This module corresponds to section 3.6 (Pixel Rectangles) of the OpenGL 1.5
-- specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.PixelRectangles (
   -- * Pixel Storage Modes
   PixelStoreDirection(..), swapBytes, lsbFirst, rowLength, skipRows,
   skipPixels, rowAlignment, imageHeight, skipImages,

   -- * Pixel Transfer Modes
   PixelTransferStage(..),
   mapColor, mapStencil, indexShift, indexOffset, depthScale, depthBias,
   rgbaScale, rgbaBias,

   -- * Pixel Maps
   PixelMapTarget(..), PixelMapComponent, PixelMap(..), GLpixelmap,
   maxPixelMapTable, pixelMap, pixelMapIToRGBA, pixelMapRGBAToRGBA,

   -- * Color Tables
   colorTableEnabled, colorTableScale, colorTableBias, colorTableFormat,
   colorTableWidth,
   colorTableRGBASizes, colorTableLuminanceSize, colorTableIntesitySize,

   -- * Convolution Filter
   maxConvolutionSize,

   -- * Drawing Pixels
   PixelFormat(..), drawPixels,

   -- * Pixel Zoom
   pixelZoom
) where

import Control.Monad ( liftM, liftM2 )
import Data.List ( zipWith4 )
import Data.Word
import Foreign.ForeignPtr ( ForeignPtr, mallocForeignPtrArray, withForeignPtr )
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Marshal.Array ( allocaArray, peekArray, pokeArray, withArray )
import Foreign.Marshal.Utils ( with )
import Foreign.Ptr ( Ptr )
import Foreign.Storable ( Storable(..) )
import Graphics.Rendering.OpenGL.GL.Capability (
   EnableCap(CapColorTable,CapPostConvolutionColorTable,
             CapPostColorMatrixColorTable),
   makeCapability, marshalCapability, unmarshalCapability )
import Graphics.Rendering.OpenGL.GL.BasicTypes (
   GLenum, GLushort, GLint, GLuint, GLsizei, GLfloat, Capability )
import Graphics.Rendering.OpenGL.GL.CoordTrans ( Size(..) )
import Graphics.Rendering.OpenGL.GL.DataType ( marshalDataType )
import Graphics.Rendering.OpenGL.GL.VertexArrays ( DataType )
import Graphics.Rendering.OpenGL.GL.Extensions (
   FunPtr, unsafePerformIO, Invoker, getProcAddress )
import Graphics.Rendering.OpenGL.GL.GLboolean (
   GLboolean, marshalGLboolean, unmarshalGLboolean )
import Graphics.Rendering.OpenGL.GL.PixelFormat (
   PixelFormat(..), marshalPixelFormat )
import Graphics.Rendering.OpenGL.GL.QueryUtils (
   GetPName(GetUnpackSwapBytes,GetUnpackLSBFirst,GetUnpackRowLength,
            GetUnpackSkipRows,GetUnpackSkipPixels,GetUnpackAlignment,
            GetUnpackImageHeight,GetUnpackSkipImages,GetPackSwapBytes,
            GetPackLSBFirst,GetPackRowLength,GetPackSkipRows,GetPackSkipPixels,
            GetPackAlignment,GetPackImageHeight,GetPackSkipImages,
            GetMapColor,GetMapStencil,GetIndexShift,GetIndexOffset,
            GetRedScale,GetGreenScale,GetBlueScale,GetAlphaScale,GetDepthScale,
            GetRedBias,GetGreenBias,GetBlueBias,GetAlphaBias,GetDepthBias,
            GetPostConvolutionRedScale,GetPostConvolutionGreenScale,
            GetPostConvolutionBlueScale,GetPostConvolutionAlphaScale,
            GetPostConvolutionRedBias,GetPostConvolutionGreenBias,
            GetPostConvolutionBlueBias,GetPostConvolutionAlphaBias,
            GetPostColorMatrixRedScale,GetPostColorMatrixGreenScale,
            GetPostColorMatrixBlueScale,GetPostColorMatrixAlphaScale,
            GetPostColorMatrixRedBias,GetPostColorMatrixGreenBias,
            GetPostColorMatrixBlueBias,GetPostColorMatrixAlphaBias,
            GetMaxPixelMapTable,GetPixelMapIToISize,GetPixelMapSToSSize,
            GetPixelMapIToRSize,GetPixelMapIToGSize,GetPixelMapIToBSize,
            GetPixelMapIToASize,GetPixelMapRToRSize,GetPixelMapGToGSize,
            GetPixelMapBToBSize,GetPixelMapAToASize,
            GetMaxConvolutionWidth, GetMaxConvolutionHeight,
            GetZoomX,GetZoomY),
   getBoolean1, getInteger1, getSizei1, getFloat1 )
import Graphics.Rendering.OpenGL.GL.StateVar (
   HasSetter(($=)), HasGetter(get), GettableStateVar, makeGettableStateVar,
   StateVar, makeStateVar )
import Graphics.Rendering.OpenGL.GL.Texturing (
   PixelInternalFormat, unmarshalPixelInternalFormat )
import Graphics.Rendering.OpenGL.GL.VertexSpec ( Color4(..) )

--------------------------------------------------------------------------------

#include "HsOpenGLExt.h"
#include "HsOpenGLTypes.h"

--------------------------------------------------------------------------------

data PixelStoreDirection =
     Pack
   | Unpack
   deriving ( Eq, Ord, Show )

--------------------------------------------------------------------------------

data PixelStore =
     UnpackSwapBytes
   | UnpackLSBFirst
   | UnpackRowLength
   | UnpackSkipRows
   | UnpackSkipPixels
   | UnpackAlignment
   | PackSwapBytes
   | PackLSBFirst
   | PackRowLength
   | PackSkipRows
   | PackSkipPixels
   | PackAlignment
   | PackSkipImages
   | PackImageHeight
   | UnpackSkipImages
   | UnpackImageHeight

marshalPixelStore :: PixelStore -> GLenum
marshalPixelStore x = case x of
   UnpackSwapBytes -> 0xcf0
   UnpackLSBFirst -> 0xcf1
   UnpackRowLength -> 0xcf2
   UnpackSkipRows -> 0xcf3
   UnpackSkipPixels -> 0xcf4
   UnpackAlignment -> 0xcf5
   PackSwapBytes -> 0xd00
   PackLSBFirst -> 0xd01
   PackRowLength -> 0xd02
   PackSkipRows -> 0xd03
   PackSkipPixels -> 0xd04
   PackAlignment -> 0xd05
   PackSkipImages -> 0x806b
   PackImageHeight -> 0x806c
   UnpackSkipImages -> 0x806d
   UnpackImageHeight -> 0x806e

--------------------------------------------------------------------------------

swapBytes :: PixelStoreDirection -> StateVar Bool
swapBytes Pack   = pixelStoreb GetPackSwapBytes PackSwapBytes
swapBytes Unpack = pixelStoreb GetUnpackSwapBytes UnpackSwapBytes

lsbFirst :: PixelStoreDirection -> StateVar Bool
lsbFirst Pack   = pixelStoreb GetPackLSBFirst PackLSBFirst
lsbFirst Unpack = pixelStoreb GetUnpackLSBFirst UnpackLSBFirst

rowLength :: PixelStoreDirection -> StateVar GLint
rowLength Pack   = pixelStorei GetPackRowLength PackRowLength
rowLength Unpack = pixelStorei GetUnpackRowLength UnpackRowLength

skipRows :: PixelStoreDirection -> StateVar GLint
skipRows Pack   = pixelStorei GetPackSkipRows PackSkipRows
skipRows Unpack = pixelStorei GetUnpackSkipRows UnpackSkipRows

skipPixels :: PixelStoreDirection -> StateVar GLint
skipPixels Pack   = pixelStorei GetPackSkipPixels PackSkipPixels
skipPixels Unpack = pixelStorei GetUnpackSkipPixels UnpackSkipPixels

rowAlignment :: PixelStoreDirection -> StateVar GLint
rowAlignment Pack   = pixelStorei GetPackAlignment PackAlignment
rowAlignment Unpack = pixelStorei GetUnpackAlignment UnpackAlignment

imageHeight :: PixelStoreDirection -> StateVar GLint
imageHeight Pack   = pixelStorei GetPackImageHeight PackImageHeight
imageHeight Unpack = pixelStorei GetUnpackImageHeight UnpackImageHeight

skipImages :: PixelStoreDirection -> StateVar GLint
skipImages Pack   = pixelStorei GetPackSkipImages PackSkipImages
skipImages Unpack = pixelStorei GetUnpackSkipImages UnpackSkipImages

--------------------------------------------------------------------------------

pixelStoreb :: GetPName -> PixelStore -> StateVar Bool
pixelStoreb pn ps =
   makeStateVar
      (getBoolean1 unmarshalGLboolean pn)
      (glPixelStorei (marshalPixelStore ps) . fromIntegral . marshalGLboolean)

pixelStorei :: GetPName -> PixelStore -> StateVar GLint
pixelStorei pn ps =
   makeStateVar
      (getInteger1 id pn)
      (glPixelStorei (marshalPixelStore ps))

foreign import CALLCONV unsafe "glPixelStorei" glPixelStorei ::
   GLenum -> GLint -> IO ()

--------------------------------------------------------------------------------

data PixelTransfer =
     MapColor
   | MapStencil
   | IndexShift
   | IndexOffset
   | RedScale
   | RedBias
   | GreenScale
   | GreenBias
   | BlueScale
   | BlueBias
   | AlphaScale
   | AlphaBias
   | DepthScale
   | DepthBias
   | PostConvolutionRedScale
   | PostConvolutionGreenScale
   | PostConvolutionBlueScale
   | PostConvolutionAlphaScale
   | PostConvolutionRedBias
   | PostConvolutionGreenBias
   | PostConvolutionBlueBias
   | PostConvolutionAlphaBias
   | PostColorMatrixRedScale
   | PostColorMatrixGreenScale
   | PostColorMatrixBlueScale
   | PostColorMatrixAlphaScale
   | PostColorMatrixRedBias
   | PostColorMatrixGreenBias
   | PostColorMatrixBlueBias
   | PostColorMatrixAlphaBias

marshalPixelTransfer :: PixelTransfer -> GLenum
marshalPixelTransfer x = case x of
   MapColor -> 0xd10
   MapStencil -> 0xd11
   IndexShift -> 0xd12
   IndexOffset -> 0xd13
   RedScale -> 0xd14
   RedBias -> 0xd15
   GreenScale -> 0xd18
   GreenBias -> 0xd19
   BlueScale -> 0xd1a
   BlueBias -> 0xd1b
   AlphaScale -> 0xd1c
   AlphaBias -> 0xd1d
   DepthScale -> 0xd1e
   DepthBias -> 0xd1f
   PostConvolutionRedScale -> 0x801c
   PostConvolutionGreenScale -> 0x801d
   PostConvolutionBlueScale -> 0x801e
   PostConvolutionAlphaScale -> 0x801f
   PostConvolutionRedBias -> 0x8020
   PostConvolutionGreenBias -> 0x8021
   PostConvolutionBlueBias -> 0x8022
   PostConvolutionAlphaBias -> 0x8023
   PostColorMatrixRedScale -> 0x80b4
   PostColorMatrixGreenScale -> 0x80b5
   PostColorMatrixBlueScale -> 0x80b6
   PostColorMatrixAlphaScale -> 0x80b7
   PostColorMatrixRedBias -> 0x80b8
   PostColorMatrixGreenBias -> 0x80b9
   PostColorMatrixBlueBias -> 0x80ba
   PostColorMatrixAlphaBias -> 0x80bb

--------------------------------------------------------------------------------

data PixelTransferStage =
     PreConvolution
   | PostConvolution
   | PostColorMatrix
   deriving ( Eq, Ord, Show )

stageToTarget :: PixelTransferStage -> ColorTableTarget
stageToTarget s = case s of
   PreConvolution  -> ColorTable
   PostConvolution -> PostConvolutionColorTable
   PostColorMatrix -> PostColorMatrixColorTable

stageToEnableCap :: PixelTransferStage -> EnableCap
stageToEnableCap s = case s of
   PreConvolution  -> CapColorTable
   PostConvolution -> CapPostConvolutionColorTable
   PostColorMatrix -> CapPostColorMatrixColorTable

stageToGetScales ::
      PixelTransferStage
   -> (GetPName, GetPName, GetPName, GetPName)
stageToGetScales s = case s of
   PreConvolution  -> (GetRedScale,
                       GetGreenScale,
                       GetBlueScale,
                       GetAlphaScale)
   PostConvolution -> (GetPostConvolutionRedScale,
                       GetPostConvolutionGreenScale,
                       GetPostConvolutionBlueScale,
                       GetPostConvolutionAlphaScale)
   PostColorMatrix -> (GetPostColorMatrixRedScale,
                       GetPostColorMatrixGreenScale,
                       GetPostColorMatrixBlueScale,
                       GetPostColorMatrixAlphaScale)

stageToSetScales ::
      PixelTransferStage
   -> (PixelTransfer, PixelTransfer, PixelTransfer, PixelTransfer)
stageToSetScales s = case s of
   PreConvolution  -> (RedScale,
                       GreenScale,
                       BlueScale,
                       AlphaScale)
   PostConvolution -> (PostConvolutionRedScale,
                       PostConvolutionGreenScale,
                       PostConvolutionBlueScale,
                       PostConvolutionAlphaScale)
   PostColorMatrix -> (PostColorMatrixRedScale,
                       PostColorMatrixGreenScale,
                       PostColorMatrixBlueScale,
                       PostColorMatrixAlphaScale)

stageToGetBiases ::
      PixelTransferStage
   -> (GetPName, GetPName, GetPName, GetPName)
stageToGetBiases s = case s of
   PreConvolution  -> (GetRedBias,
                       GetGreenBias,
                       GetBlueBias,
                       GetAlphaBias)
   PostConvolution -> (GetPostConvolutionRedBias,
                       GetPostConvolutionGreenBias,
                       GetPostConvolutionBlueBias,
                       GetPostConvolutionAlphaBias)
   PostColorMatrix -> (GetPostColorMatrixRedBias,
                       GetPostColorMatrixGreenBias,
                       GetPostColorMatrixBlueBias,
                       GetPostColorMatrixAlphaBias)

stageToSetBiases ::
      PixelTransferStage
   -> (PixelTransfer, PixelTransfer, PixelTransfer, PixelTransfer)
stageToSetBiases s = case s of
   PreConvolution  -> (RedBias,
                       GreenBias,
                       BlueBias,
                       AlphaBias)
   PostConvolution -> (PostConvolutionRedBias,
                       PostConvolutionGreenBias,
                       PostConvolutionBlueBias,
                       PostConvolutionAlphaBias)
   PostColorMatrix -> (PostColorMatrixRedBias,
                       PostColorMatrixGreenBias,
                       PostColorMatrixBlueBias,
                       PostColorMatrixAlphaBias)

--------------------------------------------------------------------------------

mapColor :: StateVar Capability
mapColor = pixelTransferb GetMapColor MapColor

mapStencil :: StateVar Capability
mapStencil = pixelTransferb GetMapStencil MapStencil

indexShift :: StateVar GLint
indexShift = pixelTransferi GetIndexShift IndexShift

indexOffset :: StateVar GLint
indexOffset = pixelTransferi GetIndexOffset IndexOffset

depthScale :: StateVar GLfloat
depthScale = pixelTransferf GetDepthScale DepthScale

depthBias :: StateVar GLfloat
depthBias = pixelTransferf GetDepthBias DepthBias

rgbaScale :: PixelTransferStage -> StateVar (Color4 GLfloat)
rgbaScale s = pixelTransfer4f (stageToGetScales s) (stageToSetScales s)

rgbaBias :: PixelTransferStage -> StateVar (Color4 GLfloat)
rgbaBias s = pixelTransfer4f (stageToGetBiases s) (stageToSetBiases s)

--------------------------------------------------------------------------------

pixelTransferb :: GetPName -> PixelTransfer -> StateVar Capability
pixelTransferb pn pt =
   makeStateVar
      (getBoolean1 unmarshalCapability pn)
      (glPixelTransferi (marshalPixelTransfer pt) .
       fromIntegral . marshalCapability)

pixelTransferi :: GetPName -> PixelTransfer -> StateVar GLint
pixelTransferi pn pt =
   makeStateVar
      (getInteger1 id pn)
      (glPixelTransferi (marshalPixelTransfer pt))

foreign import CALLCONV unsafe "glPixelTransferi" glPixelTransferi ::
   GLenum -> GLint -> IO ()

pixelTransferf :: GetPName -> PixelTransfer -> StateVar GLfloat
pixelTransferf pn pt =
   makeStateVar
      (getFloat1 id pn)
      (glPixelTransferf (marshalPixelTransfer pt))

pixelTransfer4f ::
      (GetPName,      GetPName,      GetPName,      GetPName)
   -> (PixelTransfer, PixelTransfer, PixelTransfer, PixelTransfer)
   -> StateVar (Color4 GLfloat)
pixelTransfer4f (pr, pg, pb, pa) (tr, tg, tb, ta) = makeStateVar get4f set4f
   where get4f = do
            r <- getFloat1 id pr
            g <- getFloat1 id pg
            b <- getFloat1 id pb
            a <- getFloat1 id pa
            return $ Color4 r g b a
         set4f (Color4 r g b a) = do
            glPixelTransferf (marshalPixelTransfer tr) r
            glPixelTransferf (marshalPixelTransfer tg) g
            glPixelTransferf (marshalPixelTransfer tb) b
            glPixelTransferf (marshalPixelTransfer ta) a

foreign import CALLCONV unsafe "glPixelTransferf" glPixelTransferf ::
   GLenum -> GLfloat -> IO ()

--------------------------------------------------------------------------------

data PixelMapTarget =
     IToI
   | SToS
   | IToR
   | IToG
   | IToB
   | IToA
   | RToR
   | GToG
   | BToB
   | AToA

marshalPixelMapTarget :: PixelMapTarget -> GLenum
marshalPixelMapTarget x = case x of
   IToI -> 0xc70
   SToS -> 0xc71
   IToR -> 0xc72
   IToG -> 0xc73
   IToB -> 0xc74
   IToA -> 0xc75
   RToR -> 0xc76
   GToG -> 0xc77
   BToB -> 0xc78
   AToA -> 0xc79

pixelMapTargetToGetPName :: PixelMapTarget -> GetPName
pixelMapTargetToGetPName x = case x of
   IToI -> GetPixelMapIToISize
   SToS -> GetPixelMapSToSSize
   IToR -> GetPixelMapIToRSize
   IToG -> GetPixelMapIToGSize
   IToB -> GetPixelMapIToBSize
   IToA -> GetPixelMapIToASize
   RToR -> GetPixelMapRToRSize
   GToG -> GetPixelMapGToGSize
   BToB -> GetPixelMapBToBSize
   AToA -> GetPixelMapAToASize

--------------------------------------------------------------------------------

maxPixelMapTable :: GettableStateVar GLsizei
maxPixelMapTable = makeGettableStateVar $ getSizei1 id GetMaxPixelMapTable

--------------------------------------------------------------------------------

class Storable c => PixelMapComponent c where
   getPixelMapv :: GLenum -> Ptr c -> IO ()
   pixelMapv :: GLenum -> GLsizei -> Ptr c -> IO ()

instance PixelMapComponent GLushort_ where
   getPixelMapv = glGetPixelMapusv
   pixelMapv = glPixelMapusv

foreign import CALLCONV unsafe "glGetPixelMapusv" glGetPixelMapusv ::
   GLenum -> Ptr GLushort -> IO ()

foreign import CALLCONV unsafe "glPixelMapusv" glPixelMapusv ::
   GLenum -> GLsizei -> Ptr GLushort -> IO ()

instance PixelMapComponent GLuint_ where
   getPixelMapv = glGetPixelMapuiv
   pixelMapv = glPixelMapuiv

foreign import CALLCONV unsafe "glGetPixelMapuiv" glGetPixelMapuiv ::
   GLenum -> Ptr GLuint -> IO ()

foreign import CALLCONV unsafe "glPixelMapuiv" glPixelMapuiv ::
   GLenum -> GLsizei -> Ptr GLuint -> IO ()

instance PixelMapComponent GLfloat_ where
   getPixelMapv = glGetPixelMapfv
   pixelMapv = glPixelMapfv

foreign import CALLCONV unsafe "glGetPixelMapfv" glGetPixelMapfv ::
   GLenum -> Ptr GLfloat -> IO ()

foreign import CALLCONV unsafe "glPixelMapfv" glPixelMapfv ::
   GLenum -> GLsizei -> Ptr GLfloat -> IO ()

--------------------------------------------------------------------------------

class PixelMap m where
   withNewPixelMap ::
      PixelMapComponent c => GLsizei -> (Ptr c -> IO ()) -> IO (m c)
   withPixelMap ::
      PixelMapComponent c => m c -> (GLsizei -> Ptr c -> IO a) -> IO a
   newPixelMap :: PixelMapComponent c => [c] -> IO (m c)
   getPixelMapComponents :: PixelMapComponent c => m c -> IO [c]

   withNewPixelMap size act =
      allocaArray (fromIntegral size) $ \p -> do
         act p
         components <- peekArray (fromIntegral size) p
         newPixelMap components

   withPixelMap m act = do
      components <- getPixelMapComponents m
      withArray components $ act (fromIntegral (length components))

   newPixelMap elements =
      withNewPixelMap (fromIntegral (length elements)) $ flip pokeArray elements

   getPixelMapComponents m =
      withPixelMap m $ (peekArray . fromIntegral)

--------------------------------------------------------------------------------

data GLpixelmap a = GLpixelmap GLsizei (ForeignPtr a)
#ifdef __HADDOCK__
-- Help Haddock a bit, because it doesn't do any instance inference.
instance Eq (GLpixelmap a)
instance Ord (GLpixelmap a)
instance Show (GLpixelmap a)
#else
   deriving ( Eq, Ord, Show )
#endif

instance PixelMap GLpixelmap where
   withNewPixelMap size f = do
      fp <- mallocForeignPtrArray (fromIntegral size)
      withForeignPtr fp f
      return $ GLpixelmap size fp

   withPixelMap (GLpixelmap size fp) f = withForeignPtr fp (f size)

--------------------------------------------------------------------------------

pixelMap :: (PixelMap m, PixelMapComponent c) => PixelMapTarget -> StateVar (m c)
pixelMap pm =
   makeStateVar
      (do size <- pixelMapSize pm
          withNewPixelMap size $ getPixelMapv (marshalPixelMapTarget pm))
      (\theMap -> withPixelMap theMap $ pixelMapv (marshalPixelMapTarget pm))

pixelMapSize :: PixelMapTarget -> IO GLsizei
pixelMapSize = getInteger1 fromIntegral . pixelMapTargetToGetPName

--------------------------------------------------------------------------------

-- | Convenience state variable

pixelMapIToRGBA :: PixelMapComponent c => StateVar [Color4 c]
pixelMapIToRGBA = pixelMapXToY (IToR, IToG, IToB, IToA)

-- | Convenience state variable

pixelMapRGBAToRGBA :: PixelMapComponent c => StateVar [Color4 c]
pixelMapRGBAToRGBA = pixelMapXToY (RToR, GToG, BToB, AToA)

pixelMapXToY :: PixelMapComponent c =>
      (PixelMapTarget, PixelMapTarget, PixelMapTarget, PixelMapTarget)
   -> StateVar [Color4 c]
pixelMapXToY targets =
   makeStateVar (getPixelMapXToY targets) (setPixelMapXToY targets)

getPixelMapXToY :: PixelMapComponent c
   => (PixelMapTarget, PixelMapTarget, PixelMapTarget, PixelMapTarget)
   -> IO [Color4 c]
getPixelMapXToY (toR, toG, toB, toA) = do
   withPixelMapFor toR $ \sizeR bufR ->
      withPixelMapFor toG $ \sizeG bufG ->
         withPixelMapFor toB $ \sizeB bufB ->
            withPixelMapFor toA $ \sizeA bufA -> do
               let maxSize = sizeR `max` sizeG `max` sizeB `max` sizeA
               r <- sample bufR sizeR maxSize
               g <- sample bufG sizeR maxSize
               b <- sample bufB sizeR maxSize
               a <- sample bufA sizeR maxSize
               return $ zipWith4 Color4 r g b a

withPixelMapFor ::
    PixelMapComponent c => PixelMapTarget -> (GLsizei -> Ptr c -> IO a) -> IO a
withPixelMapFor target f = do
    theMap <- get (pixelMap target)
    withGLpixelmap theMap f

withGLpixelmap :: PixelMapComponent c
               => GLpixelmap c -> (GLsizei -> Ptr c -> IO a) -> IO a
withGLpixelmap = withPixelMap

sample :: Storable a => Ptr a -> GLsizei -> GLsizei -> IO [a]
sample ptr len newLen = f (fromIntegral (newLen - 1)) []
   where scale :: Float
         scale = fromIntegral len / fromIntegral newLen
         f l acc | l < 0     = return acc
                 | otherwise = do e <- peekElemOff ptr (truncate (l * scale))
                                  f (l - 1) (e : acc)

setPixelMapXToY :: PixelMapComponent c
   => (PixelMapTarget, PixelMapTarget, PixelMapTarget, PixelMapTarget)
   -> [Color4 c] -> IO ()
setPixelMapXToY (toR, toG, toB, toA) colors = do
   (pixelMap toR $=) =<< newGLpixelmap [ r | Color4 r _ _ _ <- colors ]
   (pixelMap toG $=) =<< newGLpixelmap [ g | Color4 _ g _ _ <- colors ]
   (pixelMap toB $=) =<< newGLpixelmap [ b | Color4 _ _ b _ <- colors ]
   (pixelMap toA $=) =<< newGLpixelmap [ a | Color4 _ _ _ a <- colors ]

newGLpixelmap :: PixelMapComponent c => [c] -> IO (GLpixelmap c)
newGLpixelmap = newPixelMap

--------------------------------------------------------------------------------

colorTableEnabled :: PixelTransferStage -> StateVar Capability
colorTableEnabled = makeCapability . stageToEnableCap

--------------------------------------------------------------------------------

EXTENSION_ENTRY("GL_ARB_imaging",glColorTable,GLenum -> GLenum -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())

EXTENSION_ENTRY("GL_ARB_imaging",glColorSubTable,GLenum -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())

EXTENSION_ENTRY("GL_ARB_imaging",glCopyColorTable,GLenum -> GLenum -> GLint -> GLint -> GLsizei -> IO ())

EXTENSION_ENTRY("GL_ARB_imaging",glCopyColorSubTable,GLenum -> GLsizei -> GLint -> GLint -> GLsizei -> IO ())

EXTENSION_ENTRY("GL_ARB_imaging",glGetColorTable,GLenum -> GLenum -> GLenum -> Ptr a -> IO ())

--------------------------------------------------------------------------------

data ColorTableTarget =
     ColorTable
   | PostConvolutionColorTable
   | PostColorMatrixColorTable
   | ProxyColorTable
   | ProxyPostConvolutionColorTable
   | ProxyPostColorMatrixColorTable

marshalColorTableTarget :: ColorTableTarget -> GLenum
marshalColorTableTarget x = case x of
   ColorTable -> 0x80d0
   PostConvolutionColorTable -> 0x80d1
   PostColorMatrixColorTable -> 0x80d2
   ProxyColorTable -> 0x80d3
   ProxyPostConvolutionColorTable -> 0x80d4
   ProxyPostColorMatrixColorTable -> 0x80d5

--------------------------------------------------------------------------------

data ColorTablePName =
     ColorTableScale
   | ColorTableBias
   | ColorTableFormat
   | ColorTableWidth
   | ColorTableRedSize
   | ColorTableGreenSize
   | ColorTableBlueSize
   | ColorTableAlphaSize
   | ColorTableLuminanceSize
   | ColorTableIntensitySize

marshalColorTablePName :: ColorTablePName -> GLenum
marshalColorTablePName x = case x of
   ColorTableScale -> 0x80d6
   ColorTableBias -> 0x80d7
   ColorTableFormat -> 0x80d8
   ColorTableWidth -> 0x80d9
   ColorTableRedSize -> 0x80da
   ColorTableGreenSize -> 0x80db
   ColorTableBlueSize -> 0x80dc
   ColorTableAlphaSize -> 0x80dd
   ColorTableLuminanceSize -> 0x80de
   ColorTableIntensitySize -> 0x80df

--------------------------------------------------------------------------------

colorTableScale :: PixelTransferStage -> StateVar (Color4 GLfloat)
colorTableScale = colorTableScaleBias ColorTableScale

colorTableBias :: PixelTransferStage -> StateVar (Color4 GLfloat)
colorTableBias = colorTableScaleBias ColorTableBias

colorTableScaleBias ::
   ColorTablePName -> PixelTransferStage -> StateVar (Color4 GLfloat)
colorTableScaleBias p stage =
   makeStateVar (getColorTableParameterC4 t p) (colorTableParameterC4 t p)
   where t = stageToTarget stage

getColorTableParameterC4 ::
   ColorTableTarget -> ColorTablePName -> IO (Color4 GLfloat) 
getColorTableParameterC4 t p =
   alloca $ \buf -> do
      glGetColorTableParameterfv
         (marshalColorTableTarget t)
         (marshalColorTablePName p)
         buf
      peek buf

EXTENSION_ENTRY("GL_ARB_imaging",glGetColorTableParameterfv,GLenum -> GLenum -> Ptr (Color4 GLfloat) -> IO ())

colorTableParameterC4 ::
   ColorTableTarget -> ColorTablePName -> Color4 GLfloat -> IO ()
colorTableParameterC4 t p c =
   with c $
      glColorTableParameterfv
         (marshalColorTableTarget t)
         (marshalColorTablePName p)

EXTENSION_ENTRY("GL_ARB_imaging",glColorTableParameterfv,GLenum -> GLenum -> Ptr (Color4 GLfloat) -> IO ())

--------------------------------------------------------------------------------

colorTableFormat :: PixelTransferStage -> GettableStateVar PixelInternalFormat
colorTableFormat stage =
   makeGettableStateVar $
      liftM (unmarshalPixelInternalFormat . fromIntegral) $
         getColorTableParameteri (stageToTarget stage) ColorTableFormat

getColorTableParameteri :: ColorTableTarget -> ColorTablePName -> IO GLsizei
getColorTableParameteri t p =
   alloca $ \buf -> do
      glGetColorTableParameteriv
         (marshalColorTableTarget t)
         (marshalColorTablePName p)
         buf
      liftM fromIntegral $ peek buf

EXTENSION_ENTRY("GL_ARB_imaging",glGetColorTableParameteriv,GLenum -> GLenum -> Ptr GLint -> IO ())

--------------------------------------------------------------------------------

colorTableWidth :: PixelTransferStage -> GettableStateVar GLsizei
colorTableWidth stage =
   makeGettableStateVar $
      getColorTableParameteri (stageToTarget stage) ColorTableWidth

--------------------------------------------------------------------------------

colorTableRGBASizes ::
   PixelTransferStage -> GettableStateVar (Color4 GLsizei)
colorTableRGBASizes s =
   makeGettableStateVar $ do
      let t = stageToTarget s
      r <- getColorTableParameteri t ColorTableRedSize
      g <- getColorTableParameteri t ColorTableGreenSize
      b <- getColorTableParameteri t ColorTableBlueSize
      a <- getColorTableParameteri t ColorTableAlphaSize
      return $ Color4 r g b a

colorTableLuminanceSize ::
   PixelTransferStage -> GettableStateVar GLsizei
colorTableLuminanceSize stage =
   makeGettableStateVar $
      getColorTableParameteri (stageToTarget stage) ColorTableLuminanceSize

colorTableIntesitySize ::
   PixelTransferStage -> GettableStateVar GLsizei
colorTableIntesitySize s =
   makeGettableStateVar $
      getColorTableParameteri (stageToTarget s) ColorTableIntensitySize

--------------------------------------------------------------------------------

maxConvolutionSize :: GettableStateVar Size
maxConvolutionSize =
   makeGettableStateVar $
      liftM2 Size (getSizei1 id GetMaxConvolutionWidth)
                  (getSizei1 id GetMaxConvolutionHeight)

EXTENSION_ENTRY("GL_ARB_imaging",glConvolutionFilter1D,GLenum -> GLenum -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())

EXTENSION_ENTRY("GL_ARB_imaging",glConvolutionFilter2D,GLenum -> GLenum -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())

EXTENSION_ENTRY("GL_ARB_imaging",glConvolutionParameterf,GLenum -> GLenum -> GLfloat -> IO ())

EXTENSION_ENTRY("GL_ARB_imaging",glConvolutionParameterfv,GLenum -> GLenum -> Ptr GLfloat -> IO ())

EXTENSION_ENTRY("GL_ARB_imaging",glConvolutionParameteri,GLenum -> GLenum -> GLint -> IO ())

EXTENSION_ENTRY("GL_ARB_imaging",glConvolutionParameteriv,GLenum -> GLenum -> Ptr GLint -> IO ())

EXTENSION_ENTRY("GL_ARB_imaging",glCopyConvolutionFilter1D,GLenum -> GLenum -> GLint -> GLint -> GLsizei -> IO ())

EXTENSION_ENTRY("GL_ARB_imaging",glCopyConvolutionFilter2D,GLenum -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> IO ())

EXTENSION_ENTRY("GL_ARB_imaging",glGetConvolutionFilter,GLenum -> GLenum -> GLenum -> Ptr a -> IO ())

EXTENSION_ENTRY("GL_ARB_imaging",glGetConvolutionParameterfv,GLenum -> GLenum -> Ptr GLfloat -> IO ())

EXTENSION_ENTRY("GL_ARB_imaging",glGetConvolutionParameteriv,GLenum -> GLenum -> Ptr GLint -> IO ())

EXTENSION_ENTRY("GL_ARB_imaging",glSeparableFilter2D,GLenum -> GLenum -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr a -> Ptr a -> IO ())

EXTENSION_ENTRY("GL_ARB_imaging",glGetSeparableFilter,GLenum -> GLenum -> GLenum -> Ptr a -> Ptr a -> Ptr a -> IO ())

--------------------------------------------------------------------------------

EXTENSION_ENTRY("GL_ARB_imaging",glHistogram,GLenum -> GLsizei -> GLenum -> GLboolean -> IO ())

EXTENSION_ENTRY("GL_ARB_imaging",glResetHistogram,GLenum -> IO ())

EXTENSION_ENTRY("GL_ARB_imaging",glGetHistogram,GLenum -> GLboolean -> GLenum -> GLenum -> Ptr a -> IO ())

EXTENSION_ENTRY("GL_ARB_imaging",glGetHistogramParameterfv,GLenum -> GLenum -> Ptr GLfloat -> IO ())

EXTENSION_ENTRY("GL_ARB_imaging",glGetHistogramParameteriv,GLenum -> GLenum -> Ptr GLint -> IO ())

--------------------------------------------------------------------------------

EXTENSION_ENTRY("GL_ARB_imaging",glMinmax,GLenum -> GLenum -> GLboolean -> IO ())

EXTENSION_ENTRY("GL_ARB_imaging",glResetMinmax,GLenum -> IO ())

EXTENSION_ENTRY("GL_ARB_imaging",glGetMinmax,GLenum -> GLboolean -> GLenum -> GLenum -> Ptr a -> IO ())

EXTENSION_ENTRY("GL_ARB_imaging",glGetMinmaxParameterfv,GLenum -> GLenum -> Ptr GLfloat -> IO ())

EXTENSION_ENTRY("GL_ARB_imaging",glGetMinmaxParameteriv,GLenum -> GLenum -> Ptr GLint -> IO ())

--------------------------------------------------------------------------------

drawPixels :: Size -> PixelFormat -> DataType -> Ptr a -> IO ()
drawPixels (Size w h) f t =
   glDrawPixels w h (marshalPixelFormat f) (marshalDataType t)

foreign import CALLCONV unsafe "glDrawPixels" glDrawPixels ::
   GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ()

--------------------------------------------------------------------------------

pixelZoom :: StateVar (GLfloat, GLfloat)
pixelZoom =
   makeStateVar
      (liftM2 (,) (getFloat1 id GetZoomX) (getFloat1 id GetZoomY))
      (uncurry glPixelZoom)

foreign import CALLCONV unsafe "glPixelZoom" glPixelZoom ::
   GLfloat -> GLfloat -> IO ()
