--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.PixelRectangles.Histogram
-- Copyright   :  (c) Sven Panne 2002-2013
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

module Graphics.Rendering.OpenGL.GL.PixelRectangles.Histogram (
   Sink(..), histogram, Reset(..), getHistogram, resetHistogram,
   histogramRGBASizes, histogramLuminanceSize
) where

import Data.StateVar
import Foreign.Marshal.Utils
import Graphics.Rendering.OpenGL.GL.Capability
import Graphics.Rendering.OpenGL.GL.PeekPoke
import Graphics.Rendering.OpenGL.GL.PixelData
import Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable
import Graphics.Rendering.OpenGL.GL.PixelRectangles.Reset
import Graphics.Rendering.OpenGL.GL.PixelRectangles.Sink
import Graphics.Rendering.OpenGL.GL.Texturing.PixelInternalFormat
import Graphics.Rendering.OpenGL.GL.VertexSpec
import Graphics.Rendering.OpenGL.Raw

--------------------------------------------------------------------------------

data HistogramTarget =
     Histogram
   | ProxyHistogram

marshalHistogramTarget :: HistogramTarget -> GLenum
marshalHistogramTarget x = case x of
   Histogram -> gl_HISTOGRAM
   ProxyHistogram -> gl_PROXY_HISTOGRAM

proxyToHistogramTarget :: Proxy -> HistogramTarget
proxyToHistogramTarget x = case x of
   NoProxy -> Histogram
   Proxy -> ProxyHistogram

--------------------------------------------------------------------------------

histogram :: Proxy -> StateVar (Maybe (GLsizei, PixelInternalFormat, Sink))
histogram proxy =
   makeStateVarMaybe
      (return CapHistogram) (getHistogram' proxy) (setHistogram proxy)

getHistogram' :: Proxy -> IO (GLsizei, PixelInternalFormat, Sink)
getHistogram' proxy = do
   w <- getHistogramParameteri fromIntegral proxy HistogramWidth
   f <- getHistogramParameteri unmarshalPixelInternalFormat proxy HistogramFormat
   s <- getHistogramParameteri unmarshalSink proxy HistogramSink
   return (w, f, s)

getHistogramParameteri ::
   (GLint -> a) -> Proxy -> GetHistogramParameterPName -> IO a
getHistogramParameteri f proxy p =
   with 0 $ \buf -> do
      glGetHistogramParameteriv
         (marshalHistogramTarget (proxyToHistogramTarget proxy))
         (marshalGetHistogramParameterPName p)
         buf
      peek1 f buf

setHistogram :: Proxy -> (GLsizei, PixelInternalFormat, Sink) -> IO ()
setHistogram proxy (w, int, sink) =
   glHistogram
      (marshalHistogramTarget (proxyToHistogramTarget proxy))
      w
      (marshalPixelInternalFormat' int)
      (marshalSink sink)

--------------------------------------------------------------------------------

getHistogram :: Reset -> PixelData a -> IO ()
getHistogram reset pd =
   withPixelData pd $
      glGetHistogram
         (marshalHistogramTarget Histogram)
         (marshalReset reset)

--------------------------------------------------------------------------------

resetHistogram :: IO ()
resetHistogram = glResetHistogram (marshalHistogramTarget Histogram)

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
   HistogramWidth -> gl_HISTOGRAM_WIDTH
   HistogramFormat -> gl_HISTOGRAM_FORMAT
   HistogramRedSize -> gl_HISTOGRAM_RED_SIZE
   HistogramGreenSize -> gl_HISTOGRAM_GREEN_SIZE
   HistogramBlueSize -> gl_HISTOGRAM_BLUE_SIZE
   HistogramAlphaSize -> gl_HISTOGRAM_ALPHA_SIZE
   HistogramLuminanceSize -> gl_HISTOGRAM_LUMINANCE_SIZE
   HistogramSink -> gl_HISTOGRAM_SINK

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
      getHistogramParameteri fromIntegral proxy HistogramLuminanceSize
