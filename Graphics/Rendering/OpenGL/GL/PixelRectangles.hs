--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.PixelRectangles
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- This module corresponds to section 3.6 (Pixel Rectangles) of the OpenGL 1.4
-- specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.PixelRectangles (
   -- * Pixel Storage Modes
   unpackSwapBytes, unpackLSBFirst, unpackRowLength, unpackSkipRows,
   unpackSkipPixels, unpackAlignment, unpackImageHeight, unpackSkipImages,
   packSwapBytes, packLSBFirst, packRowLength, packSkipRows,
   packSkipPixels, packAlignment, packImageHeight, packSkipImages,

   -- * Pixel Transfer Modes
   mapColor, mapStencil, indexShift, indexOffset, rgbaScale, depthScale,
   rgbaBias, depthBias, postConvolutionRGBAScale, postConvolutionRGBABias,
   postColorMatrixRGBAScale, postColorMatrixRGBABias,

   PixelMap, PixelMapComponent, withNewPixelMap, withPixelMap,
   maxPixelMapTable,
   pixelMapIToI, pixelMapSToS, pixelMapIToR, pixelMapIToG, pixelMapIToB,
   pixelMapIToA, pixelMapRToR, pixelMapGToG, pixelMapBToB, pixelMapAToA,

   colorTableEnabled, postConvolutionColorTableEnabled,
   postColorMatrixColorTableEnabled,
   colorTableScale, colorTableBias,
   postConvolutionColorTableScale, postConvolutionColorTableBias,
   postColorMatrixColorTableScale, postColorMatrixColorTableBias,

   PixelFormat(..), drawPixels
) where

import Foreign.ForeignPtr ( ForeignPtr, mallocForeignPtrArray, withForeignPtr )
import Foreign.Marshal.Alloc ( alloca )
import Foreign.Marshal.Utils ( with )
import Foreign.Ptr ( Ptr )
import Foreign.Storable ( Storable(peek) )
import Graphics.Rendering.OpenGL.GL.BasicTypes (
   GLenum, GLint, GLuint, GLsizei, GLfloat )
import Graphics.Rendering.OpenGL.GL.Capability (
   EnableCap(CapColorTable,CapPostConvolutionColorTable,
             CapPostColorMatrixColorTable),
   makeCapability )
import Graphics.Rendering.OpenGL.GL.CoordTrans ( Size(..) )
import Graphics.Rendering.OpenGL.GL.DataType ( DataType(..), marshalDataType )
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
            GetPixelMapBToBSize,GetPixelMapAToASize),
   getBoolean1, getInteger1, getSizei1, getFloat1 )
import Graphics.Rendering.OpenGL.GL.StateVar (
   GettableStateVar, makeGettableStateVar, StateVar, makeStateVar )
import Graphics.Rendering.OpenGL.GL.VertexSpec ( Color4(..) )

--------------------------------------------------------------------------------

#include "HsOpenGLExt.h"

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

unpackSwapBytes :: StateVar Bool
unpackSwapBytes = pixelStoreb GetUnpackSwapBytes UnpackSwapBytes

unpackLSBFirst :: StateVar Bool
unpackLSBFirst = pixelStoreb GetUnpackLSBFirst UnpackLSBFirst

unpackRowLength :: StateVar GLint
unpackRowLength = pixelStorei GetUnpackRowLength UnpackRowLength

unpackSkipRows :: StateVar GLint
unpackSkipRows = pixelStorei GetUnpackSkipRows UnpackSkipRows

unpackSkipPixels :: StateVar GLint
unpackSkipPixels = pixelStorei GetUnpackSkipPixels UnpackSkipPixels

unpackAlignment :: StateVar GLint
unpackAlignment = pixelStorei GetUnpackAlignment UnpackAlignment

unpackImageHeight :: StateVar GLint
unpackImageHeight = pixelStorei GetUnpackImageHeight UnpackImageHeight

unpackSkipImages :: StateVar GLint
unpackSkipImages = pixelStorei GetUnpackSkipImages UnpackSkipImages

packSwapBytes :: StateVar Bool
packSwapBytes = pixelStoreb GetPackSwapBytes PackSwapBytes

packLSBFirst :: StateVar Bool
packLSBFirst = pixelStoreb GetPackLSBFirst PackLSBFirst

packRowLength :: StateVar GLint
packRowLength = pixelStorei GetPackRowLength PackRowLength

packSkipRows :: StateVar GLint
packSkipRows = pixelStorei GetPackSkipRows PackSkipRows

packSkipPixels :: StateVar GLint
packSkipPixels = pixelStorei GetPackSkipPixels PackSkipPixels

packAlignment :: StateVar GLint
packAlignment = pixelStorei GetPackAlignment PackAlignment

packImageHeight :: StateVar GLint
packImageHeight = pixelStorei GetPackImageHeight PackImageHeight

packSkipImages :: StateVar GLint
packSkipImages = pixelStorei GetPackSkipImages PackSkipImages

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

mapColor :: StateVar Bool
mapColor = pixelTransferb GetMapColor MapColor

mapStencil :: StateVar Bool
mapStencil = pixelTransferb GetMapStencil MapStencil

indexShift :: StateVar GLint
indexShift = pixelTransferi GetIndexShift IndexShift

indexOffset :: StateVar GLint
indexOffset = pixelTransferi GetIndexOffset IndexOffset

rgbaScale :: StateVar (Color4 GLfloat)
rgbaScale =
    pixelTransfer4f GetRedScale GetGreenScale GetBlueScale GetAlphaScale
                       RedScale    GreenScale    BlueScale    AlphaScale

depthScale :: StateVar GLfloat
depthScale = pixelTransferf GetDepthScale DepthScale

rgbaBias :: StateVar (Color4 GLfloat)
rgbaBias =
    pixelTransfer4f GetRedBias GetGreenBias GetBlueBias GetAlphaBias
                       RedBias    GreenBias    BlueBias    AlphaBias

depthBias :: StateVar GLfloat
depthBias = pixelTransferf GetDepthBias DepthBias

postConvolutionRGBAScale :: StateVar (Color4 GLfloat)
postConvolutionRGBAScale =
    pixelTransfer4f GetPostConvolutionRedScale  GetPostConvolutionGreenScale
                    GetPostConvolutionBlueScale GetPostConvolutionAlphaScale
                       PostConvolutionRedScale     PostConvolutionGreenScale
                       PostConvolutionBlueScale    PostConvolutionAlphaScale

postConvolutionRGBABias :: StateVar (Color4 GLfloat)
postConvolutionRGBABias =
    pixelTransfer4f GetPostConvolutionRedBias  GetPostConvolutionGreenBias
                    GetPostConvolutionBlueBias GetPostConvolutionAlphaBias
                       PostConvolutionRedBias     PostConvolutionGreenBias
                       PostConvolutionBlueBias    PostConvolutionAlphaBias

postColorMatrixRGBAScale :: StateVar (Color4 GLfloat)
postColorMatrixRGBAScale =
    pixelTransfer4f GetPostColorMatrixRedScale  GetPostColorMatrixGreenScale
                    GetPostColorMatrixBlueScale GetPostColorMatrixAlphaScale
                       PostColorMatrixRedScale     PostColorMatrixGreenScale
                       PostColorMatrixBlueScale    PostColorMatrixAlphaScale

postColorMatrixRGBABias :: StateVar (Color4 GLfloat)
postColorMatrixRGBABias =
    pixelTransfer4f GetPostColorMatrixRedBias  GetPostColorMatrixGreenBias
                    GetPostColorMatrixBlueBias GetPostColorMatrixAlphaBias
                       PostColorMatrixRedBias     PostColorMatrixGreenBias
                       PostColorMatrixBlueBias    PostColorMatrixAlphaBias

--------------------------------------------------------------------------------

pixelTransferb :: GetPName -> PixelTransfer -> StateVar Bool
pixelTransferb pn pt =
   makeStateVar
      (getBoolean1 unmarshalGLboolean pn)
      (glPixelTransferi (marshalPixelTransfer pt) .
       fromIntegral . marshalGLboolean)

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
      GetPName      -> GetPName      -> GetPName      -> GetPName
   -> PixelTransfer -> PixelTransfer -> PixelTransfer -> PixelTransfer
   -> StateVar (Color4 GLfloat)
pixelTransfer4f pr pg pb pa tr tg tb ta = makeStateVar get4f set4f
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

data PixelMap' =
     PixelMapIToI
   | PixelMapSToS
   | PixelMapIToR
   | PixelMapIToG
   | PixelMapIToB
   | PixelMapIToA
   | PixelMapRToR
   | PixelMapGToG
   | PixelMapBToB
   | PixelMapAToA

marshalPixelMap :: PixelMap' -> GLenum
marshalPixelMap x = case x of
   PixelMapIToI -> 0xc70
   PixelMapSToS -> 0xc71
   PixelMapIToR -> 0xc72
   PixelMapIToG -> 0xc73
   PixelMapIToB -> 0xc74
   PixelMapIToA -> 0xc75
   PixelMapRToR -> 0xc76
   PixelMapGToG -> 0xc77
   PixelMapBToB -> 0xc78
   PixelMapAToA -> 0xc79

pixelMapToGetPName :: PixelMap' -> GetPName
pixelMapToGetPName x = case x of
   PixelMapIToI -> GetPixelMapIToISize
   PixelMapSToS -> GetPixelMapSToSSize
   PixelMapIToR -> GetPixelMapIToRSize
   PixelMapIToG -> GetPixelMapIToGSize
   PixelMapIToB -> GetPixelMapIToBSize
   PixelMapIToA -> GetPixelMapIToASize
   PixelMapRToR -> GetPixelMapRToRSize
   PixelMapGToG -> GetPixelMapGToGSize
   PixelMapBToB -> GetPixelMapBToBSize
   PixelMapAToA -> GetPixelMapAToASize

--------------------------------------------------------------------------------

maxPixelMapTable :: GettableStateVar GLsizei
maxPixelMapTable = makeGettableStateVar (getSizei1 id GetMaxPixelMapTable)

--------------------------------------------------------------------------------

data PixelMap a = PixelMap GLsizei (ForeignPtr a)
   deriving ( Eq, Ord, Show )

withNewPixelMap ::
   PixelMapComponent a => GLsizei -> (Ptr a -> IO ()) -> IO (PixelMap a)
withNewPixelMap size f = do
   fp <- mallocForeignPtrArray (fromIntegral size)
   withForeignPtr fp f
   return $ PixelMap size fp

withPixelMap :: PixelMap a -> (GLsizei -> Ptr a -> IO b) -> IO b
withPixelMap (PixelMap size fp) f = withForeignPtr fp (f size)

--------------------------------------------------------------------------------

pixelMapIToI :: StateVar (PixelMap GLuint)
pixelMapIToI = pixelMap PixelMapIToI

pixelMapSToS :: StateVar (PixelMap (GLuint))
pixelMapSToS = pixelMap PixelMapSToS

pixelMapIToR :: StateVar (PixelMap (GLfloat))
pixelMapIToR = pixelMap PixelMapIToR

pixelMapIToG :: StateVar (PixelMap (GLfloat))
pixelMapIToG = pixelMap PixelMapIToG

pixelMapIToB :: StateVar (PixelMap (GLfloat))
pixelMapIToB = pixelMap PixelMapIToB

pixelMapIToA :: StateVar (PixelMap (GLfloat))
pixelMapIToA = pixelMap PixelMapIToA

pixelMapRToR :: StateVar (PixelMap (GLfloat))
pixelMapRToR = pixelMap PixelMapRToR

pixelMapGToG :: StateVar (PixelMap (GLfloat))
pixelMapGToG = pixelMap PixelMapGToG

pixelMapBToB :: StateVar (PixelMap (GLfloat))
pixelMapBToB = pixelMap PixelMapBToB

pixelMapAToA :: StateVar (PixelMap (GLfloat))
pixelMapAToA = pixelMap PixelMapAToA

pixelMap :: PixelMapComponent a => PixelMap' -> StateVar (PixelMap a)
pixelMap pm =
   makeStateVar
      (do size <- getInteger1 fromIntegral (pixelMapToGetPName pm)
          withNewPixelMap size $ getPixelMapv (marshalPixelMap pm))
      (\theMap -> withPixelMap theMap $ pixelMapv (marshalPixelMap pm))

class Storable a => PixelMapComponent a where
   getPixelMapv :: GLenum -> Ptr a -> IO ()
   pixelMapv :: GLenum -> GLsizei -> Ptr a -> IO ()

instance PixelMapComponent GLuint where
   getPixelMapv = glGetPixelMapuiv
   pixelMapv = glPixelMapuiv

foreign import CALLCONV unsafe "glGetPixelMapuiv" glGetPixelMapuiv ::
   GLenum -> Ptr GLuint -> IO ()

foreign import CALLCONV unsafe "glPixelMapuiv" glPixelMapuiv ::
   GLenum -> GLsizei -> Ptr GLuint -> IO ()

instance PixelMapComponent GLfloat where
   getPixelMapv = glGetPixelMapfv
   pixelMapv = glPixelMapfv

foreign import CALLCONV unsafe "glGetPixelMapfv" glGetPixelMapfv ::
   GLenum -> Ptr GLfloat -> IO ()

foreign import CALLCONV unsafe "glPixelMapfv" glPixelMapfv ::
   GLenum -> GLsizei -> Ptr GLfloat -> IO ()

--------------------------------------------------------------------------------

colorTableEnabled :: StateVar Bool
colorTableEnabled = makeCapability CapColorTable

postConvolutionColorTableEnabled :: StateVar Bool
postConvolutionColorTableEnabled = makeCapability CapPostConvolutionColorTable

postColorMatrixColorTableEnabled :: StateVar Bool
postColorMatrixColorTableEnabled = makeCapability CapPostColorMatrixColorTable

--------------------------------------------------------------------------------

EXTENSION_ENTRY("GL_ARB_imaging",glColorTable,GLenum -> GLenum -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())

EXTENSION_ENTRY("GL_ARB_imaging",glColorSubTable,GLenum -> GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ())

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

unmarshalColorTableTarget :: GLenum -> ColorTableTarget
unmarshalColorTableTarget x
   | x == 0x80d0 = ColorTable
   | x == 0x80d1 = PostConvolutionColorTable
   | x == 0x80d2 = PostColorMatrixColorTable
   | x == 0x80d3 = ProxyColorTable
   | x == 0x80d4 = ProxyPostConvolutionColorTable
   | x == 0x80d5 = ProxyPostColorMatrixColorTable
   | otherwise = error ("unmarshalColorTableTarget: illegal value " ++ show x)

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

colorTableScale :: StateVar (Color4 GLfloat)
colorTableScale = colorTableScaleBias ColorTable ColorTableScale

colorTableBias :: StateVar (Color4 GLfloat)
colorTableBias = colorTableScaleBias ColorTable ColorTableBias

postConvolutionColorTableScale :: StateVar (Color4 GLfloat)
postConvolutionColorTableScale =
   colorTableScaleBias PostConvolutionColorTable ColorTableScale

postConvolutionColorTableBias :: StateVar (Color4 GLfloat)
postConvolutionColorTableBias =
   colorTableScaleBias PostConvolutionColorTable ColorTableBias

postColorMatrixColorTableScale :: StateVar (Color4 GLfloat)
postColorMatrixColorTableScale =
   colorTableScaleBias PostColorMatrixColorTable ColorTableScale

postColorMatrixColorTableBias :: StateVar (Color4 GLfloat)
postColorMatrixColorTableBias =
   colorTableScaleBias PostColorMatrixColorTable ColorTableBias

colorTableScaleBias ::
   ColorTableTarget -> ColorTablePName -> StateVar (Color4 GLfloat)
colorTableScaleBias t p =
   makeStateVar (getColorTableParameterC4 t p) (colorTableParameterC4 t p)

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

EXTENSION_ENTRY("GL_ARB_imaging",glGetColorTableParameteriv,GLenum -> GLenum -> Ptr GLint -> IO ())

EXTENSION_ENTRY("GL_ARB_imaging",glCopyColorSubTable,GLenum -> GLsizei -> GLint -> GLint -> GLsizei -> IO ())

EXTENSION_ENTRY("GL_ARB_imaging",glCopyColorTable,GLenum -> GLenum -> GLint -> GLint -> GLsizei -> IO ())

EXTENSION_ENTRY("GL_ARB_imaging",glGetColorTable,GLenum -> GLenum -> GLenum -> Ptr a -> IO ())

EXTENSION_ENTRY("GL_ARB_imaging",glHistogram,GLenum -> GLsizei -> GLenum -> GLboolean -> IO ())

EXTENSION_ENTRY("GL_ARB_imaging",glResetHistogram,GLenum -> IO ())

EXTENSION_ENTRY("GL_ARB_imaging",glGetHistogram,GLenum -> GLboolean -> GLenum -> GLenum -> Ptr a -> IO ())

EXTENSION_ENTRY("GL_ARB_imaging",glGetHistogramParameterfv,GLenum -> GLenum -> Ptr GLfloat -> IO ())

EXTENSION_ENTRY("GL_ARB_imaging",glGetHistogramParameteriv,GLenum -> GLenum -> Ptr GLint -> IO ())

EXTENSION_ENTRY("GL_ARB_imaging",glMinmax,GLenum -> GLenum -> GLboolean -> IO ())

EXTENSION_ENTRY("GL_ARB_imaging",glResetMinmax,GLenum -> IO ())

EXTENSION_ENTRY("GL_ARB_imaging",glGetMinmax,GLenum -> GLboolean -> GLenum -> GLenum -> Ptr a -> IO ())

EXTENSION_ENTRY("GL_ARB_imaging",glGetMinmaxParameterfv,GLenum -> GLenum -> Ptr GLfloat -> IO ())

EXTENSION_ENTRY("GL_ARB_imaging",glGetMinmaxParameteriv,GLenum -> GLenum -> Ptr GLint -> IO ())

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

drawPixels :: Size -> PixelFormat -> DataType -> Ptr a -> IO ()
drawPixels (Size w h) f t =
   glDrawPixels w h (marshalPixelFormat f) (marshalDataType t)

foreign import CALLCONV unsafe "glDrawPixels" glDrawPixels ::
   GLsizei -> GLsizei -> GLenum -> GLenum -> Ptr a -> IO ()
