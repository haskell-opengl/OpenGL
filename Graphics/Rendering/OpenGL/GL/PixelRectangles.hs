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
   ColorTableStage(..), colorTableStage,
   Proxy(..), ColorTable(..), colorTable, getColorTable,
   copyColorTable, colorSubTable, copyColorSubTable,
   colorTableScale, colorTableBias, colorTableFormat, colorTableWidth,
   colorTableRGBASizes, colorTableLuminanceSize, colorTableIntesitySize,

   -- * Convolution Filter
   ConvolutionTarget(..), convolution,
   convolutionFilter1D, getConvolutionFilter1D,
   convolutionFilter2D, getConvolutionFilter2D,
   separableFilter2D, getSeparableFilter2D,
   copyConvolutionFilter1D, copyConvolutionFilter2D,
   convolutionWidth, convolutionHeight,
   maxConvolutionWidth, maxConvolutionHeight,
   ConvolutionBorderMode(..), convolutionBorderMode,
   convolutionFilterScale, convolutionFilterBias,

   -- * Histogram Table
   Sink(..), histogram, Reset(..), getHistogram, resetHistogram,
   histogramRGBASizes, histogramLuminanceSize,

   -- * Minmax Table
   minmax, getMinmax, resetMinmax,

   -- * Drawing Pixels
   PixelData(..), PixelFormat(..), drawPixels,

   -- * Pixel Zoom
   pixelZoom
) where

import Control.Monad ( liftM2 )
import Data.List ( zipWith4 )
import Data.Word
import Foreign.ForeignPtr ( ForeignPtr, mallocForeignPtrArray, withForeignPtr )
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Marshal.Array ( allocaArray, peekArray, pokeArray, withArray )
import Foreign.Marshal.Utils ( with )
import Foreign.Ptr ( Ptr, nullPtr )
import Foreign.Storable ( Storable(..) )
import Graphics.Rendering.OpenGL.GL.Capability (
   EnableCap(CapColorTable,CapPostConvolutionColorTable,
             CapPostColorMatrixColorTable,CapTextureColorTable,
             CapConvolution1D, CapConvolution2D,CapSeparable2D,
             CapHistogram,CapMinmax),
   makeCapability, marshalCapability, unmarshalCapability, makeStateVarMaybe )
import Graphics.Rendering.OpenGL.GL.BasicTypes (
   GLenum, GLushort, GLint, GLuint, GLsizei, GLfloat, Capability )
import Graphics.Rendering.OpenGL.GL.CoordTrans ( Position(..), Size(..) )
import Graphics.Rendering.OpenGL.GL.Extensions (
   FunPtr, unsafePerformIO, Invoker, getProcAddress )
import Graphics.Rendering.OpenGL.GL.GLboolean (
   GLboolean, marshalGLboolean, unmarshalGLboolean )
import Graphics.Rendering.OpenGL.GL.PeekPoke ( peek1 )
import Graphics.Rendering.OpenGL.GL.PixelData ( PixelData(..), withPixelData )
import Graphics.Rendering.OpenGL.GL.PixelFormat ( PixelFormat(..) )
import Graphics.Rendering.OpenGL.GL.PixelInternalFormat (
   PixelInternalFormat, marshalPixelInternalFormat,
   unmarshalPixelInternalFormat )
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
            GetZoomX,GetZoomY),
   getBoolean1, getInteger1, getSizei1, getFloat1 )
import Graphics.Rendering.OpenGL.GL.StateVar (
   HasSetter(($=)), HasGetter(get), GettableStateVar, makeGettableStateVar,
   StateVar, makeStateVar )
import Graphics.Rendering.OpenGL.GL.VertexSpec ( Color4(..) )
import Graphics.Rendering.OpenGL.GLU.ErrorsInternal (
   recordInvalidEnum, recordInvalidValue )

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

data ColorTableStage =
     ColorTableStage
   | PostConvolutionColorTableStage
   | PostColorMatrixColorTableStage
   | TextureColorTableStage
   deriving ( Eq, Ord, Show )

colorTableStageToColorTable :: ColorTableStage -> ColorTable
colorTableStageToColorTable x = case x of
   ColorTableStage -> ColorTable
   PostConvolutionColorTableStage -> PostConvolutionColorTable
   PostColorMatrixColorTableStage -> PostColorMatrixColorTable
   TextureColorTableStage -> TextureColorTable

colorTableStageToEnableCap :: ColorTableStage -> EnableCap
colorTableStageToEnableCap x = case x of
   ColorTableStage -> CapColorTable
   PostConvolutionColorTableStage -> CapPostConvolutionColorTable
   PostColorMatrixColorTableStage -> CapPostColorMatrixColorTable
   TextureColorTableStage -> CapTextureColorTable

--------------------------------------------------------------------------------

colorTableStage :: ColorTableStage -> StateVar Capability
colorTableStage = makeCapability . colorTableStageToEnableCap

--------------------------------------------------------------------------------

data ColorTable =
     ColorTable
   | PostConvolutionColorTable
   | PostColorMatrixColorTable
   | Texture1DColorTable
   | Texture2DColorTable
   | Texture3DColorTable
   | TextureCubeMapColorTable
   | TextureColorTable
   | SharedTexturePalette
   deriving ( Eq, Ord, Show )

marshalColorTable :: ColorTable -> GLenum
marshalColorTable x = case x of
   ColorTable -> 0x80d0
   PostConvolutionColorTable -> 0x80d1
   PostColorMatrixColorTable -> 0x80d2
   Texture1DColorTable -> 0xde0
   Texture2DColorTable -> 0xde1
   Texture3DColorTable -> 0x806f
   TextureCubeMapColorTable -> 0x8513
   TextureColorTable -> 0x80bc
   SharedTexturePalette -> 0x81fb

--------------------------------------------------------------------------------

data Proxy =
     NoProxy
   | Proxy
   deriving ( Eq, Ord, Show )

--------------------------------------------------------------------------------

marshalProxyColorTable :: Proxy -> ColorTable -> Maybe GLenum
marshalProxyColorTable NoProxy x = Just (marshalColorTable x)
marshalProxyColorTable Proxy   x = case x of
   ColorTable -> Just 0x80d3
   PostConvolutionColorTable -> Just 0x80d4
   PostColorMatrixColorTable -> Just 0x80d5
   Texture1DColorTable -> Just 0x8063
   Texture2DColorTable -> Just 0x8064
   Texture3DColorTable -> Just 0x8070
   TextureCubeMapColorTable -> Just 0x851b
   TextureColorTable -> Just 0x80bd
   SharedTexturePalette -> Nothing

--------------------------------------------------------------------------------

colorTable ::
   Proxy -> ColorTable -> PixelInternalFormat -> GLsizei -> PixelData a -> IO ()
colorTable proxy ct int w pd =
   maybe recordInvalidEnum
         (\target -> withPixelData pd $
            glColorTable target (marshalPixelInternalFormat int) w)
         (marshalProxyColorTable proxy ct)

EXTENSION_ENTRY("GL_ARB_imaging",glColorTable,GLenum -> GLenum -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())

--------------------------------------------------------------------------------

getColorTable :: ColorTable -> PixelData a -> IO ()
getColorTable ct pd =
   withPixelData pd $ glGetColorTable (marshalColorTable ct)

EXTENSION_ENTRY("GL_ARB_imaging",glGetColorTable,GLenum -> GLenum -> GLenum -> Ptr a -> IO ())

--------------------------------------------------------------------------------

copyColorTable :: ColorTable -> PixelInternalFormat -> Position -> GLsizei -> IO ()
copyColorTable ct int (Position x y) =
   glCopyColorTable (marshalColorTable ct) (marshalPixelInternalFormat int) x y

EXTENSION_ENTRY("GL_ARB_imaging",glCopyColorTable,GLenum -> GLenum -> GLint -> GLint -> GLsizei -> IO ())

--------------------------------------------------------------------------------

colorSubTable :: ColorTable -> GLsizei -> GLsizei -> PixelData a -> IO ()
colorSubTable ct start count pd =
   withPixelData pd $ glColorSubTable (marshalColorTable ct) start count

EXTENSION_ENTRY("GL_ARB_imaging",glColorSubTable,GLenum -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())

--------------------------------------------------------------------------------

copyColorSubTable :: ColorTable -> GLsizei -> Position -> GLsizei -> IO ()
copyColorSubTable ct start (Position x y) =
   glCopyColorSubTable (marshalColorTable ct) start x y

EXTENSION_ENTRY("GL_ARB_imaging",glCopyColorSubTable,GLenum -> GLsizei -> GLint -> GLint -> GLsizei -> IO ())

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

colorTableScale :: ColorTableStage -> StateVar (Color4 GLfloat)
colorTableScale = colorTableScaleBias ColorTableScale

colorTableBias :: ColorTableStage -> StateVar (Color4 GLfloat)
colorTableBias = colorTableScaleBias ColorTableBias

colorTableScaleBias ::
   ColorTablePName -> ColorTableStage -> StateVar (Color4 GLfloat)
colorTableScaleBias p s =
   makeStateVar (getColorTableParameterC4f ct p) (colorTableParameterC4f ct p)
   where ct = colorTableStageToColorTable s

getColorTableParameterC4f ::
   ColorTable -> ColorTablePName -> IO (Color4 GLfloat) 
getColorTableParameterC4f ct p =
   alloca $ \buf -> do
      glGetColorTableParameterfv
         (marshalColorTable ct)
         (marshalColorTablePName p)
         buf
      peek buf

EXTENSION_ENTRY("GL_ARB_imaging",glGetColorTableParameterfv,GLenum -> GLenum -> Ptr (Color4 GLfloat) -> IO ())

colorTableParameterC4f ::
   ColorTable -> ColorTablePName -> Color4 GLfloat -> IO ()
colorTableParameterC4f ct p c =
   with c $
      glColorTableParameterfv (marshalColorTable ct) (marshalColorTablePName p)

EXTENSION_ENTRY("GL_ARB_imaging",glColorTableParameterfv,GLenum -> GLenum -> Ptr (Color4 GLfloat) -> IO ())

--------------------------------------------------------------------------------

colorTableFormat :: ColorTable -> GettableStateVar PixelInternalFormat
colorTableFormat ct =
   makeGettableStateVar $
      getColorTableParameteri unmarshalPixelInternalFormat' ct ColorTableFormat

unmarshalPixelInternalFormat' :: GLint -> PixelInternalFormat
unmarshalPixelInternalFormat' = unmarshalPixelInternalFormat . fromIntegral

getColorTableParameteri :: (GLint -> a) -> ColorTable -> ColorTablePName -> IO a
getColorTableParameteri f ct p =
   alloca $ \buf -> do
      glGetColorTableParameteriv
         (marshalColorTable ct)
         (marshalColorTablePName p)
         buf
      peek1 f buf

EXTENSION_ENTRY("GL_ARB_imaging",glGetColorTableParameteriv,GLenum -> GLenum -> Ptr GLint -> IO ())

--------------------------------------------------------------------------------

colorTableWidth :: ColorTable -> GettableStateVar GLsizei
colorTableWidth ct =
   makeGettableStateVar $
      getColorTableParameteri fromIntegral ct ColorTableWidth

--------------------------------------------------------------------------------

colorTableRGBASizes :: ColorTable -> GettableStateVar (Color4 GLsizei)
colorTableRGBASizes ct =
   makeGettableStateVar $ do
      r <- getColorTableParameteri fromIntegral ct ColorTableRedSize
      g <- getColorTableParameteri fromIntegral ct ColorTableGreenSize
      b <- getColorTableParameteri fromIntegral ct ColorTableBlueSize
      a <- getColorTableParameteri fromIntegral ct ColorTableAlphaSize
      return $ Color4 r g b a

colorTableLuminanceSize :: ColorTable -> GettableStateVar GLsizei
colorTableLuminanceSize ct =
   makeGettableStateVar $
      getColorTableParameteri fromIntegral ct ColorTableLuminanceSize

colorTableIntesitySize :: ColorTable -> GettableStateVar GLsizei
colorTableIntesitySize ct =
   makeGettableStateVar $
      getColorTableParameteri fromIntegral ct ColorTableIntensitySize

--------------------------------------------------------------------------------

data ConvolutionTarget =
     Convolution1D
   | Convolution2D
   | Separable2D
   deriving ( Eq, Ord, Show )

marshalConvolutionTarget :: ConvolutionTarget -> GLenum
marshalConvolutionTarget x = case x of
   Convolution1D -> 0x8010
   Convolution2D -> 0x8011
   Separable2D -> 0x8012

convolutionTargetToEnableCap :: ConvolutionTarget -> EnableCap
convolutionTargetToEnableCap x = case x of
   Convolution1D -> CapConvolution1D
   Convolution2D -> CapConvolution2D
   Separable2D -> CapSeparable2D

--------------------------------------------------------------------------------

convolution :: ConvolutionTarget -> StateVar Capability
convolution = makeCapability . convolutionTargetToEnableCap

--------------------------------------------------------------------------------

convolutionFilter1D :: PixelInternalFormat -> GLsizei -> PixelData a -> IO ()
convolutionFilter1D int w pd =
   withPixelData pd $
      glConvolutionFilter1D
         (marshalConvolutionTarget Convolution1D)
         (marshalPixelInternalFormat int) w

EXTENSION_ENTRY("GL_ARB_imaging",glConvolutionFilter1D,GLenum -> GLenum -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())

--------------------------------------------------------------------------------

getConvolutionFilter1D :: PixelData a -> IO ()
getConvolutionFilter1D = getConvolutionFilter Convolution1D

getConvolutionFilter :: ConvolutionTarget -> PixelData a -> IO ()
getConvolutionFilter t pd =
   withPixelData pd $ glGetConvolutionFilter (marshalConvolutionTarget t)

EXTENSION_ENTRY("GL_ARB_imaging",glGetConvolutionFilter,GLenum -> GLenum -> GLenum -> Ptr a -> IO ())

--------------------------------------------------------------------------------

convolutionFilter2D :: PixelInternalFormat -> Size -> PixelData a -> IO ()
convolutionFilter2D int (Size w h) pd =
   withPixelData pd $
      glConvolutionFilter2D
         (marshalConvolutionTarget Convolution2D)
         (marshalPixelInternalFormat int) w h

EXTENSION_ENTRY("GL_ARB_imaging",glConvolutionFilter2D,GLenum -> GLenum -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())

--------------------------------------------------------------------------------

getConvolutionFilter2D :: PixelData a -> IO ()
getConvolutionFilter2D = getConvolutionFilter Convolution2D

--------------------------------------------------------------------------------

separableFilter2D ::
   PixelInternalFormat -> Size -> PixelData a -> PixelData a -> IO ()
separableFilter2D int (Size w h) pdRow pdCol =
   withPixelData pdRow $ \f1 d1 p1 ->
      withPixelData pdCol $ \f2 d2 p2 ->
         if f1 == f2 && d1 == d2
            then glSeparableFilter2D
                    (marshalConvolutionTarget Separable2D)
                    (marshalPixelInternalFormat int) w h f1 d1 p1 p2
            else recordInvalidValue

EXTENSION_ENTRY("GL_ARB_imaging",glSeparableFilter2D,GLenum -> GLenum -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr a -> Ptr a -> IO ())

--------------------------------------------------------------------------------

getSeparableFilter2D :: PixelData a -> PixelData a -> IO ()
getSeparableFilter2D pdRow pdCol =
   withPixelData pdRow $ \f1 d1 p1 ->
      withPixelData pdCol $ \f2 d2 p2 ->
         if f1 == f2 && d1 == d2
            then glGetSeparableFilter
                    (marshalConvolutionTarget Separable2D) f1 d1 p1 p2 nullPtr
            else recordInvalidValue

EXTENSION_ENTRY("GL_ARB_imaging",glGetSeparableFilter,GLenum -> GLenum -> GLenum -> Ptr a -> Ptr a -> Ptr a -> IO ())

--------------------------------------------------------------------------------

copyConvolutionFilter1D :: PixelInternalFormat -> Position -> GLsizei -> IO ()
copyConvolutionFilter1D int (Position x y) =
   glCopyConvolutionFilter1D
      (marshalConvolutionTarget Convolution1D) (marshalPixelInternalFormat int)
      x y

EXTENSION_ENTRY("GL_ARB_imaging",glCopyConvolutionFilter1D,GLenum -> GLenum -> GLint -> GLint -> GLsizei -> IO ())

--------------------------------------------------------------------------------

copyConvolutionFilter2D :: PixelInternalFormat -> Position -> Size -> IO ()
copyConvolutionFilter2D int (Position x y) (Size w h) =
   glCopyConvolutionFilter2D
      (marshalConvolutionTarget Convolution2D) (marshalPixelInternalFormat int)
      x y w h

EXTENSION_ENTRY("GL_ARB_imaging",glCopyConvolutionFilter2D,GLenum -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> IO ())

--------------------------------------------------------------------------------

data ConvolutionParameter =
     ConvolutionBorderColor
   | ConvolutionBorderMode
   | ConvolutionFilterScale
   | ConvolutionFilterBias
   | ConvolutionFormat
   | ConvolutionWidth
   | ConvolutionHeight
   | MaxConvolutionWidth
   | MaxConvolutionHeight
   deriving ( Eq, Ord, Show )

marshalConvolutionParameter :: ConvolutionParameter -> GLenum
marshalConvolutionParameter x = case x of
   ConvolutionBorderColor -> 0x8154
   ConvolutionBorderMode -> 0x8013
   ConvolutionFilterScale -> 0x8014
   ConvolutionFilterBias -> 0x8015
   ConvolutionFormat -> 0x8017
   ConvolutionWidth -> 0x8018
   ConvolutionHeight -> 0x8019
   MaxConvolutionWidth -> 0x801a
   MaxConvolutionHeight -> 0x801b

--------------------------------------------------------------------------------

convolutionWidth :: ConvolutionTarget -> GettableStateVar GLsizei
convolutionWidth t = convolutionParameteri t ConvolutionWidth

convolutionHeight :: ConvolutionTarget -> GettableStateVar GLsizei
convolutionHeight t = convolutionParameteri t ConvolutionHeight

maxConvolutionWidth :: ConvolutionTarget -> GettableStateVar GLsizei
maxConvolutionWidth t = convolutionParameteri t MaxConvolutionWidth

maxConvolutionHeight :: ConvolutionTarget -> GettableStateVar GLsizei
maxConvolutionHeight t = convolutionParameteri t MaxConvolutionHeight

convolutionParameteri ::
   ConvolutionTarget -> ConvolutionParameter -> GettableStateVar GLsizei
convolutionParameteri t p =
   makeGettableStateVar (getConvolutionParameteri fromIntegral t p)

getConvolutionParameteri ::
   (GLint -> a) -> ConvolutionTarget -> ConvolutionParameter -> IO a
getConvolutionParameteri f t p =
   alloca $ \buf -> do
      glGetConvolutionParameteriv
         (marshalConvolutionTarget t) (marshalConvolutionParameter p) buf
      peek1 f buf

EXTENSION_ENTRY("GL_ARB_imaging",glGetConvolutionParameteriv,GLenum -> GLenum -> Ptr GLint -> IO ())

--------------------------------------------------------------------------------

data ConvolutionBorderMode' =
     Reduce'
   | ConstantBorder'
   | ReplicateBorder'

marshalConvolutionBorderMode' :: ConvolutionBorderMode' -> GLint
marshalConvolutionBorderMode' x = case x of
   Reduce' -> 0x8016
   ConstantBorder' -> 0x8151
   ReplicateBorder' -> 0x8153

unmarshalConvolutionBorderMode' :: GLint -> ConvolutionBorderMode'
unmarshalConvolutionBorderMode' x
   | x == 0x8016 = Reduce'
   | x == 0x8151 = ConstantBorder'
   | x == 0x8153 = ReplicateBorder'
   | otherwise = error ("unmarshalConvolutionBorderMode': illegal value " ++ show x)

--------------------------------------------------------------------------------

data ConvolutionBorderMode =
     Reduce
   | ConstantBorder (Color4 GLfloat)
   | ReplicateBorder
   deriving ( Eq, Ord, Show )

--------------------------------------------------------------------------------

convolutionBorderMode :: ConvolutionTarget -> StateVar ConvolutionBorderMode
convolutionBorderMode t =
   makeStateVar (getConvolutionBorderMode t) (setConvolutionBorderMode t)

getConvolutionBorderMode :: ConvolutionTarget -> IO ConvolutionBorderMode
getConvolutionBorderMode t = do
   mode <- getConvolutionParameteri
              unmarshalConvolutionBorderMode' t ConvolutionBorderMode
   case mode of
      Reduce' -> return Reduce
      ConstantBorder' -> do
         c <- getConvolutionParameterC4f t ConvolutionBorderColor
         return $ ConstantBorder c
      ReplicateBorder' -> return ReplicateBorder

setConvolutionBorderMode :: ConvolutionTarget -> ConvolutionBorderMode -> IO ()
setConvolutionBorderMode t mode = do
   let setBM = setConvolutionParameteri
                  marshalConvolutionBorderMode' t ConvolutionBorderMode
   case mode of
      Reduce -> setBM Reduce'
      ConstantBorder c -> do
         setBM ConstantBorder'
         convolutionParameterC4f t ConvolutionBorderColor c
      ReplicateBorder -> setBM ReplicateBorder'

setConvolutionParameteri ::
   (a -> GLint) -> ConvolutionTarget -> ConvolutionParameter -> a -> IO ()
setConvolutionParameteri f t p x =
   glConvolutionParameteri
      (marshalConvolutionTarget t) (marshalConvolutionParameter p) (f x)

EXTENSION_ENTRY("GL_ARB_imaging",glConvolutionParameteri,GLenum -> GLenum -> GLint -> IO ())

--------------------------------------------------------------------------------

convolutionFilterScale :: ConvolutionTarget -> StateVar (Color4 GLfloat)
convolutionFilterScale = convolutionC4f ConvolutionFilterScale

convolutionFilterBias :: ConvolutionTarget -> StateVar (Color4 GLfloat)
convolutionFilterBias = convolutionC4f ConvolutionFilterBias

convolutionC4f ::
   ConvolutionParameter -> ConvolutionTarget -> StateVar (Color4 GLfloat)
convolutionC4f p t =
   makeStateVar (getConvolutionParameterC4f t p) (convolutionParameterC4f t p)

getConvolutionParameterC4f ::
   ConvolutionTarget -> ConvolutionParameter -> IO (Color4 GLfloat)
getConvolutionParameterC4f t p =
   alloca $ \buf -> do
      glGetConvolutionParameterfv
         (marshalConvolutionTarget t) (marshalConvolutionParameter p) buf
      peek buf

EXTENSION_ENTRY("GL_ARB_imaging",glGetConvolutionParameterfv,GLenum -> GLenum -> Ptr (Color4 GLfloat) -> IO ())

convolutionParameterC4f ::
   ConvolutionTarget -> ConvolutionParameter -> Color4 GLfloat -> IO ()
convolutionParameterC4f t p c =
   with c $
      glConvolutionParameterfv
         (marshalConvolutionTarget t) (marshalConvolutionParameter p)

EXTENSION_ENTRY("GL_ARB_imaging",glConvolutionParameterfv,GLenum -> GLenum -> Ptr (Color4 GLfloat) -> IO ())

--------------------------------------------------------------------------------

data HistogramTarget =
     Histogram
   | ProxyHistogram

marshalHistogramTarget :: HistogramTarget -> GLenum
marshalHistogramTarget x = case x of
   Histogram -> 0x8024
   ProxyHistogram -> 0x8025

proxyToHistogramTarget :: Proxy -> HistogramTarget
proxyToHistogramTarget x = case x of
   NoProxy -> Histogram
   Proxy -> ProxyHistogram

--------------------------------------------------------------------------------

data Sink =
     PassThrough
   | Sink
   deriving ( Eq, Ord, Show )

marshalSink :: Sink -> GLboolean
marshalSink x = marshalGLboolean (x == Sink)

unmarshalSink :: GLint -> Sink
unmarshalSink s =
   if unmarshalGLboolean (fromIntegral s) then Sink else PassThrough

--------------------------------------------------------------------------------

histogram :: Proxy -> StateVar (Maybe (GLsizei, PixelInternalFormat, Sink))
histogram proxy =
   makeStateVarMaybe
      (return CapHistogram) (getHistogram' proxy) (setHistogram proxy)

getHistogram' :: Proxy -> IO (GLsizei, PixelInternalFormat, Sink)
getHistogram' proxy = do
   w <- getHistogramParameteri fromIntegral proxy HistogramWidth
   f <- getHistogramParameteri unmarshalPixelInternalFormat' proxy HistogramFormat
   s <- getHistogramParameteri unmarshalSink proxy HistogramSink
   return (w, f, s)

getHistogramParameteri ::
   (GLint -> a) -> Proxy -> GetHistogramParameterPName -> IO a
getHistogramParameteri f proxy p =
   alloca $ \buf -> do
      glGetHistogramParameteriv
         (marshalHistogramTarget (proxyToHistogramTarget proxy))
         (marshalGetHistogramParameterPName p)
         buf
      peek1 f buf

EXTENSION_ENTRY("GL_ARB_imaging",glGetHistogramParameteriv,GLenum -> GLenum -> Ptr GLint -> IO ())

setHistogram :: Proxy -> (GLsizei, PixelInternalFormat, Sink) -> IO ()
setHistogram proxy (w, int, sink) =
   glHistogram
      (marshalHistogramTarget (proxyToHistogramTarget proxy))
      w
      (marshalPixelInternalFormat int)
      (marshalSink sink)
         
EXTENSION_ENTRY("GL_ARB_imaging",glHistogram,GLenum -> GLsizei -> GLenum -> GLboolean -> IO ())

--------------------------------------------------------------------------------

data Reset =
     NoReset
   | Reset
   deriving ( Eq, Ord, Show )

marshalReset :: Reset -> GLboolean
marshalReset x = marshalGLboolean (x == Reset)

--------------------------------------------------------------------------------

getHistogram :: Reset -> PixelData a -> IO ()
getHistogram reset pd =
   withPixelData pd $
      glGetHistogram
         (marshalHistogramTarget Histogram)
         (marshalReset reset)

EXTENSION_ENTRY("GL_ARB_imaging",glGetHistogram,GLenum -> GLboolean -> GLenum -> GLenum -> Ptr a -> IO ())

--------------------------------------------------------------------------------

resetHistogram :: IO ()
resetHistogram = glResetHistogram (marshalHistogramTarget Histogram)

EXTENSION_ENTRY("GL_ARB_imaging",glResetHistogram,GLenum -> IO ())

--------------------------------------------------------------------------------

data GetHistogramParameterPName =
     HistogramWidth
   | HistogramFormat
   | HistogramRedSize
   | HistogramGreenSize
   | HistogramBlueSize
   | HistogramAlphaSize
   | HistogramLuminanceSize
   | HistogramSink

marshalGetHistogramParameterPName :: GetHistogramParameterPName -> GLenum
marshalGetHistogramParameterPName x = case x of
   HistogramWidth -> 0x8026
   HistogramFormat -> 0x8027
   HistogramRedSize -> 0x8028
   HistogramGreenSize -> 0x8029
   HistogramBlueSize -> 0x802a
   HistogramAlphaSize -> 0x802b
   HistogramLuminanceSize -> 0x802c
   HistogramSink -> 0x802d

--------------------------------------------------------------------------------

histogramRGBASizes :: Proxy -> GettableStateVar (Color4 GLsizei)
histogramRGBASizes proxy =
   makeGettableStateVar $ do
      r <- getHistogramParameteri fromIntegral proxy HistogramRedSize
      g <- getHistogramParameteri fromIntegral proxy HistogramGreenSize
      b <- getHistogramParameteri fromIntegral proxy HistogramBlueSize
      a <- getHistogramParameteri fromIntegral proxy HistogramAlphaSize
      return $ Color4 r g b a

histogramLuminanceSize :: Proxy -> GettableStateVar GLsizei
histogramLuminanceSize proxy =
   makeGettableStateVar $
      getHistogramParameteri id proxy HistogramLuminanceSize

--------------------------------------------------------------------------------

data MinmaxTarget =
     Minmax

marshalMinmaxTarget :: MinmaxTarget -> GLenum
marshalMinmaxTarget x = case x of
   Minmax -> 0x802e

--------------------------------------------------------------------------------

minmax :: StateVar (Maybe (PixelInternalFormat, Sink))
minmax = makeStateVarMaybe (return CapMinmax) getMinmax' setMinmax

getMinmax' :: IO (PixelInternalFormat, Sink)
getMinmax' = do
   f <- getMinmaxParameteri unmarshalPixelInternalFormat' MinmaxFormat
   s <- getMinmaxParameteri unmarshalSink MinmaxSink
   return (f, s)

setMinmax :: (PixelInternalFormat, Sink) -> IO ()
setMinmax (int, sink) =
   glMinmax
      (marshalMinmaxTarget Minmax)
      (marshalPixelInternalFormat int)
      (marshalSink sink)

EXTENSION_ENTRY("GL_ARB_imaging",glMinmax,GLenum -> GLenum -> GLboolean -> IO ())

--------------------------------------------------------------------------------

getMinmax :: Reset -> PixelData a -> IO ()
getMinmax reset pd =
   withPixelData pd $
      glGetMinmax (marshalMinmaxTarget Minmax) (marshalReset reset)

EXTENSION_ENTRY("GL_ARB_imaging",glGetMinmax,GLenum -> GLboolean -> GLenum -> GLenum -> Ptr a -> IO ())

--------------------------------------------------------------------------------

resetMinmax :: IO ()
resetMinmax = glResetMinmax (marshalMinmaxTarget Minmax)

EXTENSION_ENTRY("GL_ARB_imaging",glResetMinmax,GLenum -> IO ())

--------------------------------------------------------------------------------

data GetMinmaxParameterPName =
     MinmaxFormat
   | MinmaxSink

marshalGetMinmaxParameterPName :: GetMinmaxParameterPName -> GLenum
marshalGetMinmaxParameterPName x = case x of
   MinmaxFormat -> 0x802f
   MinmaxSink -> 0x8030

--------------------------------------------------------------------------------

getMinmaxParameteri :: (GLint -> a) -> GetMinmaxParameterPName -> IO a
getMinmaxParameteri f p =
   alloca $ \buf -> do
      glGetMinmaxParameteriv
         (marshalMinmaxTarget Minmax)
         (marshalGetMinmaxParameterPName p)
         buf
      peek1 f buf

EXTENSION_ENTRY("GL_ARB_imaging",glGetMinmaxParameteriv,GLenum -> GLenum -> Ptr GLint -> IO ())

--------------------------------------------------------------------------------

drawPixels :: Size -> PixelData a -> IO ()
drawPixels (Size w h) pd = withPixelData pd $ glDrawPixels w h

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
