--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.PixelRectangles.PixelTransfer
-- Copyright   :  (c) Sven Panne 2002-2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to a part of section 3.6.1 (Pixel Storage Modes) of
-- the OpenGL 2.1 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.PixelRectangles.PixelTransfer (
   PixelTransferStage(..),
   mapColor, mapStencil, indexShift, indexOffset, depthScale, depthBias,
   rgbaScale, rgbaBias
) where

import Data.StateVar
import Graphics.Rendering.OpenGL.GL.Capability
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.GL.VertexSpec
import Graphics.GL

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
   MapColor -> GL_MAP_COLOR
   MapStencil -> GL_MAP_STENCIL
   IndexShift -> GL_INDEX_SHIFT
   IndexOffset -> GL_INDEX_OFFSET
   RedScale -> GL_RED_SCALE
   RedBias -> GL_RED_BIAS
   GreenScale -> GL_GREEN_SCALE
   GreenBias -> GL_GREEN_BIAS
   BlueScale -> GL_BLUE_SCALE
   BlueBias -> GL_BLUE_BIAS
   AlphaScale -> GL_ALPHA_SCALE
   AlphaBias -> GL_ALPHA_BIAS
   DepthScale -> GL_DEPTH_SCALE
   DepthBias -> GL_DEPTH_BIAS
   PostConvolutionRedScale -> GL_POST_CONVOLUTION_RED_SCALE
   PostConvolutionGreenScale -> GL_POST_CONVOLUTION_GREEN_SCALE
   PostConvolutionBlueScale -> GL_POST_CONVOLUTION_BLUE_SCALE
   PostConvolutionAlphaScale -> GL_POST_CONVOLUTION_ALPHA_SCALE
   PostConvolutionRedBias -> GL_POST_CONVOLUTION_RED_BIAS
   PostConvolutionGreenBias -> GL_POST_CONVOLUTION_GREEN_BIAS
   PostConvolutionBlueBias -> GL_POST_CONVOLUTION_BLUE_BIAS
   PostConvolutionAlphaBias -> GL_POST_CONVOLUTION_ALPHA_BIAS
   PostColorMatrixRedScale -> GL_POST_COLOR_MATRIX_RED_SCALE
   PostColorMatrixGreenScale -> GL_POST_COLOR_MATRIX_GREEN_SCALE
   PostColorMatrixBlueScale -> GL_POST_COLOR_MATRIX_BLUE_SCALE
   PostColorMatrixAlphaScale -> GL_POST_COLOR_MATRIX_ALPHA_SCALE
   PostColorMatrixRedBias -> GL_POST_COLOR_MATRIX_RED_BIAS
   PostColorMatrixGreenBias -> GL_POST_COLOR_MATRIX_GREEN_BIAS
   PostColorMatrixBlueBias -> GL_POST_COLOR_MATRIX_BLUE_BIAS
   PostColorMatrixAlphaBias -> GL_POST_COLOR_MATRIX_ALPHA_BIAS

--------------------------------------------------------------------------------

data PixelTransferStage =
     PreConvolution
   | PostConvolution
   | PostColorMatrix
   deriving ( Eq, Ord, Show )

stageToGetScales ::
      PixelTransferStage
   -> (PName1F, PName1F, PName1F, PName1F)
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
   -> (PName1F, PName1F, PName1F, PName1F)
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

pixelTransferb :: GetPName1I p => p -> PixelTransfer -> StateVar Capability
pixelTransferb pn pt =
   makeStateVar
      (getBoolean1 unmarshalCapability pn)
      (glPixelTransferi (marshalPixelTransfer pt) .
       fromIntegral . marshalCapability)

pixelTransferi :: GetPName1I p => p -> PixelTransfer -> StateVar GLint
pixelTransferi pn pt =
   makeStateVar
      (getInteger1 id pn)
      (glPixelTransferi (marshalPixelTransfer pt))

pixelTransferf :: GetPName1F p => p -> PixelTransfer -> StateVar GLfloat
pixelTransferf pn pt =
   makeStateVar
      (getFloat1 id pn)
      (glPixelTransferf (marshalPixelTransfer pt))

pixelTransfer4f :: GetPName1F p =>
      (p, p, p, p)
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
