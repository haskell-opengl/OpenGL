--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.PixelRectangles.PixelTransfer
-- Copyright   :  (c) Sven Panne 2002-2005
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
-- This module corresponds to a part of section 3.6.1 (Pixel Storage Modes) of
-- the OpenGL 1.5 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.PixelRectangles.PixelTransfer (
   PixelTransferStage(..),
   mapColor, mapStencil, indexShift, indexOffset, depthScale, depthBias,
   rgbaScale, rgbaBias
) where

import Graphics.Rendering.OpenGL.GL.Capability (
   marshalCapability, unmarshalCapability )
import Graphics.Rendering.OpenGL.GL.BasicTypes (
   GLenum, GLint, GLfloat, Capability )
import Graphics.Rendering.OpenGL.GL.QueryUtils (
   GetPName(GetMapColor,GetMapStencil,GetIndexShift,GetIndexOffset,
            GetRedScale,GetGreenScale,GetBlueScale,GetAlphaScale,GetDepthScale,
            GetRedBias,GetGreenBias,GetBlueBias,GetAlphaBias,GetDepthBias,
            GetPostConvolutionRedScale,GetPostConvolutionGreenScale,
            GetPostConvolutionBlueScale,GetPostConvolutionAlphaScale,
            GetPostConvolutionRedBias,GetPostConvolutionGreenBias,
            GetPostConvolutionBlueBias,GetPostConvolutionAlphaBias,
            GetPostColorMatrixRedScale,GetPostColorMatrixGreenScale,
            GetPostColorMatrixBlueScale,GetPostColorMatrixAlphaScale,
            GetPostColorMatrixRedBias,GetPostColorMatrixGreenBias,
            GetPostColorMatrixBlueBias,GetPostColorMatrixAlphaBias),
   getBoolean1, getInteger1, getFloat1 )
import Graphics.Rendering.OpenGL.GL.StateVar ( StateVar, makeStateVar )
import Graphics.Rendering.OpenGL.GL.VertexSpec ( Color4(..) )

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
