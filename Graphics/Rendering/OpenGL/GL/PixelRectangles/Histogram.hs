--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.PixelRectangles.Histogram
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

module Graphics.Rendering.OpenGL.GL.PixelRectangles.Histogram (
   Sink(..), histogram, Reset(..), getHistogram, resetHistogram,
   histogramRGBASizes, histogramLuminanceSize
) where

import Foreign.Marshal.Alloc ( alloca )
import Foreign.Ptr ( Ptr )
import Graphics.Rendering.OpenGL.GL.Capability (
   EnableCap(CapHistogram), makeStateVarMaybe )
import Graphics.Rendering.OpenGL.GL.BasicTypes (
   GLboolean, GLenum, GLint, GLsizei )
import Graphics.Rendering.OpenGL.GL.Extensions (
   FunPtr, unsafePerformIO, Invoker, getProcAddress )
import Graphics.Rendering.OpenGL.GL.PeekPoke ( peek1 )
import Graphics.Rendering.OpenGL.GL.PixelRectangles.Rasterization (
    PixelData(..) )
import Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable (
   PixelInternalFormat )
import Graphics.Rendering.OpenGL.GL.PixelData ( withPixelData )
import Graphics.Rendering.OpenGL.GL.Texturing.PixelInternalFormat (
   marshalPixelInternalFormat', unmarshalPixelInternalFormat )
import Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable ( Proxy(..) )
import Graphics.Rendering.OpenGL.GL.PixelRectangles.Sink (
   Sink(..), marshalSink, unmarshalSink )
import Graphics.Rendering.OpenGL.GL.PixelRectangles.Reset (
   Reset(..), marshalReset )
import Graphics.Rendering.OpenGL.GL.StateVar (
   GettableStateVar, makeGettableStateVar, StateVar )
import Graphics.Rendering.OpenGL.GL.VertexSpec ( Color4(..) )

--------------------------------------------------------------------------------

#include "HsOpenGLExt.h"

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
      (marshalPixelInternalFormat' int)
      (marshalSink sink)
         
EXTENSION_ENTRY("GL_ARB_imaging",glHistogram,GLenum -> GLsizei -> GLenum -> GLboolean -> IO ())

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
