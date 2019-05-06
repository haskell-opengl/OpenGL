--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.PixelRectangles.Convolution
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

module Graphics.Rendering.OpenGL.GL.PixelRectangles.Convolution (
   ConvolutionTarget(..), convolution,
   convolutionFilter1D, getConvolutionFilter1D,
   convolutionFilter2D, getConvolutionFilter2D,
   separableFilter2D, getSeparableFilter2D,
   copyConvolutionFilter1D, copyConvolutionFilter2D,
   convolutionWidth, convolutionHeight,
   maxConvolutionWidth, maxConvolutionHeight,
   ConvolutionBorderMode(..), convolutionBorderMode,
   convolutionFilterScale, convolutionFilterBias,
) where

import Data.StateVar
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.OpenGL.GL.Capability
import Graphics.Rendering.OpenGL.GL.CoordTrans
import Graphics.Rendering.OpenGL.GL.PeekPoke
import Graphics.Rendering.OpenGL.GL.PixelData
import Graphics.Rendering.OpenGL.GL.Texturing.PixelInternalFormat
import Graphics.Rendering.OpenGL.GL.VertexSpec
import Graphics.Rendering.OpenGL.GLU.ErrorsInternal
import Graphics.GL

--------------------------------------------------------------------------------

data ConvolutionTarget =
     Convolution1D
   | Convolution2D
   | Separable2D
   deriving ( Eq, Ord, Show )

marshalConvolutionTarget :: ConvolutionTarget -> GLenum
marshalConvolutionTarget x = case x of
   Convolution1D -> GL_CONVOLUTION_1D
   Convolution2D -> GL_CONVOLUTION_2D
   Separable2D -> GL_SEPARABLE_2D

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
         (marshalPixelInternalFormat' int) w

--------------------------------------------------------------------------------

getConvolutionFilter1D :: PixelData a -> IO ()
getConvolutionFilter1D = getConvolutionFilter Convolution1D

getConvolutionFilter :: ConvolutionTarget -> PixelData a -> IO ()
getConvolutionFilter t pd =
   withPixelData pd $ glGetConvolutionFilter (marshalConvolutionTarget t)

--------------------------------------------------------------------------------

convolutionFilter2D :: PixelInternalFormat -> Size -> PixelData a -> IO ()
convolutionFilter2D int (Size w h) pd =
   withPixelData pd $
      glConvolutionFilter2D
         (marshalConvolutionTarget Convolution2D)
         (marshalPixelInternalFormat' int) w h

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
                    (marshalPixelInternalFormat' int) w h f1 d1 p1 p2
            else recordInvalidValue

--------------------------------------------------------------------------------

getSeparableFilter2D :: PixelData a -> PixelData a -> IO ()
getSeparableFilter2D pdRow pdCol =
   withPixelData pdRow $ \f1 d1 p1 ->
      withPixelData pdCol $ \f2 d2 p2 ->
         if f1 == f2 && d1 == d2
            then glGetSeparableFilter
                    (marshalConvolutionTarget Separable2D) f1 d1 p1 p2 nullPtr
            else recordInvalidValue

--------------------------------------------------------------------------------

copyConvolutionFilter1D :: PixelInternalFormat -> Position -> GLsizei -> IO ()
copyConvolutionFilter1D int (Position x y) =
   glCopyConvolutionFilter1D
      (marshalConvolutionTarget Convolution1D) (marshalPixelInternalFormat' int)
      x y

--------------------------------------------------------------------------------

copyConvolutionFilter2D :: PixelInternalFormat -> Position -> Size -> IO ()
copyConvolutionFilter2D int (Position x y) (Size w h) =
   glCopyConvolutionFilter2D
      (marshalConvolutionTarget Convolution2D) (marshalPixelInternalFormat' int)
      x y w h

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
   ConvolutionBorderColor -> GL_CONVOLUTION_BORDER_COLOR
   ConvolutionBorderMode -> GL_CONVOLUTION_BORDER_MODE
   ConvolutionFilterScale -> GL_CONVOLUTION_FILTER_SCALE
   ConvolutionFilterBias -> GL_CONVOLUTION_FILTER_BIAS
   ConvolutionFormat -> GL_CONVOLUTION_FORMAT
   ConvolutionWidth -> GL_CONVOLUTION_WIDTH
   ConvolutionHeight -> GL_CONVOLUTION_HEIGHT
   MaxConvolutionWidth -> GL_MAX_CONVOLUTION_WIDTH
   MaxConvolutionHeight -> GL_MAX_CONVOLUTION_HEIGHT

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
   with 0 $ \buf -> do
      glGetConvolutionParameteriv
         (marshalConvolutionTarget t) (marshalConvolutionParameter p) buf
      peek1 f buf

--------------------------------------------------------------------------------

data ConvolutionBorderMode' =
     Reduce'
   | ConstantBorder'
   | ReplicateBorder'

marshalConvolutionBorderMode' :: ConvolutionBorderMode' -> GLint
marshalConvolutionBorderMode' x = fromIntegral $ case x of
   Reduce' -> GL_REDUCE
   ConstantBorder' -> GL_CONSTANT_BORDER
   ReplicateBorder' -> GL_REPLICATE_BORDER

unmarshalConvolutionBorderMode' :: GLint -> ConvolutionBorderMode'
unmarshalConvolutionBorderMode' x
   | y == GL_REDUCE = Reduce'
   | y == GL_CONSTANT_BORDER = ConstantBorder'
   | y == GL_REPLICATE_BORDER = ReplicateBorder'
   | otherwise = error ("unmarshalConvolutionBorderMode': illegal value " ++ show x)
   where y = fromIntegral x

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
         (marshalConvolutionTarget t) (marshalConvolutionParameter p) (castPtr buf)
      peek buf

convolutionParameterC4f ::
   ConvolutionTarget -> ConvolutionParameter -> Color4 GLfloat -> IO ()
convolutionParameterC4f t p c =
   with c $ \ptr ->
      glConvolutionParameterfv
         (marshalConvolutionTarget t) (marshalConvolutionParameter p) (castPtr ptr)
