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
   postColorMatrixRGBAScale, postColorMatrixRGBABias, maxPixelMapTable
) where

import Graphics.Rendering.OpenGL.GL.BasicTypes (
   GLenum, GLint, GLsizei, GLfloat )
import Graphics.Rendering.OpenGL.GL.GLboolean (
   marshalGLboolean, unmarshalGLboolean )
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
            GetMaxPixelMapTable),
   getBoolean1, getInteger1, getFloat1 )
import Graphics.Rendering.OpenGL.GL.StateVar (
   GettableStateVar, makeGettableStateVar, StateVar, makeStateVar )

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

rgbaScale :: StateVar (GLfloat, GLfloat, GLfloat, GLfloat)
rgbaScale =
    pixelTransfer4f GetRedScale GetGreenScale GetBlueScale GetAlphaScale
                       RedScale    GreenScale    BlueScale    AlphaScale

depthScale :: StateVar GLfloat
depthScale = pixelTransferf GetDepthScale DepthScale

rgbaBias :: StateVar (GLfloat, GLfloat, GLfloat, GLfloat)
rgbaBias =
    pixelTransfer4f GetRedBias GetGreenBias GetBlueBias GetAlphaBias
                       RedBias    GreenBias    BlueBias    AlphaBias

depthBias :: StateVar GLfloat
depthBias = pixelTransferf GetDepthBias DepthBias

postConvolutionRGBAScale :: StateVar (GLfloat, GLfloat, GLfloat, GLfloat)
postConvolutionRGBAScale =
    pixelTransfer4f GetPostConvolutionRedScale GetPostConvolutionGreenScale GetPostConvolutionBlueScale GetPostConvolutionAlphaScale
                       PostConvolutionRedScale    PostConvolutionGreenScale    PostConvolutionBlueScale    PostConvolutionAlphaScale

postConvolutionRGBABias :: StateVar (GLfloat, GLfloat, GLfloat, GLfloat)
postConvolutionRGBABias =
    pixelTransfer4f GetPostConvolutionRedBias GetPostConvolutionGreenBias GetPostConvolutionBlueBias GetPostConvolutionAlphaBias
                       PostConvolutionRedBias    PostConvolutionGreenBias    PostConvolutionBlueBias    PostConvolutionAlphaBias

postColorMatrixRGBAScale :: StateVar (GLfloat, GLfloat, GLfloat, GLfloat)
postColorMatrixRGBAScale =
    pixelTransfer4f GetPostColorMatrixRedScale GetPostColorMatrixGreenScale GetPostColorMatrixBlueScale GetPostColorMatrixAlphaScale
                       PostColorMatrixRedScale    PostColorMatrixGreenScale    PostColorMatrixBlueScale    PostColorMatrixAlphaScale

postColorMatrixRGBABias :: StateVar (GLfloat, GLfloat, GLfloat, GLfloat)
postColorMatrixRGBABias =
    pixelTransfer4f GetPostColorMatrixRedBias GetPostColorMatrixGreenBias GetPostColorMatrixBlueBias GetPostColorMatrixAlphaBias
                       PostColorMatrixRedBias    PostColorMatrixGreenBias    PostColorMatrixBlueBias    PostColorMatrixAlphaBias

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
      GetPName -> GetPName -> GetPName -> GetPName
   -> PixelTransfer -> PixelTransfer -> PixelTransfer -> PixelTransfer
   -> StateVar (GLfloat, GLfloat, GLfloat, GLfloat)
pixelTransfer4f pn1 pn2 pn3 pn4 pt1 pt2 pt3 pt4 = makeStateVar get4f set4f
   where get4f = do
            x1 <- getFloat1 id pn1
            x2 <- getFloat1 id pn2
            x3 <- getFloat1 id pn3
            x4 <- getFloat1 id pn4
            return (x1, x2, x3, x4)
         set4f (x1, x2, x3, x4) = do
            glPixelTransferf (marshalPixelTransfer pt1) x1
            glPixelTransferf (marshalPixelTransfer pt2) x2
            glPixelTransferf (marshalPixelTransfer pt3) x3
            glPixelTransferf (marshalPixelTransfer pt4) x4

foreign import CALLCONV unsafe "glPixelTransferf" glPixelTransferf ::
   GLenum -> GLfloat -> IO ()

--------------------------------------------------------------------------------

maxPixelMapTable :: GettableStateVar GLsizei
maxPixelMapTable =
   makeGettableStateVar (getInteger1 fromIntegral GetMaxPixelMapTable)
