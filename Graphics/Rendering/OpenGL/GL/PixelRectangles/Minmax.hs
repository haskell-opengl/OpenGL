--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.PixelRectangles.Minmax
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

module Graphics.Rendering.OpenGL.GL.PixelRectangles.Minmax (
   minmax, getMinmax, resetMinmax
) where

import Foreign.Marshal.Alloc ( alloca )
import Foreign.Ptr ( Ptr )
import Graphics.Rendering.OpenGL.GL.Capability (
   EnableCap(CapMinmax), makeStateVarMaybe )
import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLboolean, GLenum, GLint )
import Graphics.Rendering.OpenGL.GL.Extensions (
   FunPtr, unsafePerformIO, Invoker, getProcAddress )
import Graphics.Rendering.OpenGL.GL.PeekPoke ( peek1 )
import Graphics.Rendering.OpenGL.GL.PixelRectangles.ColorTable (
   PixelInternalFormat )
import Graphics.Rendering.OpenGL.GL.PixelRectangles.Rasterization (
    PixelData(..) )
import Graphics.Rendering.OpenGL.GL.PixelData ( withPixelData )
import Graphics.Rendering.OpenGL.GL.Texturing.PixelInternalFormat (
   marshalPixelInternalFormat', unmarshalPixelInternalFormat )
import Graphics.Rendering.OpenGL.GL.PixelRectangles.Histogram (
   Reset(..), Sink )
import Graphics.Rendering.OpenGL.GL.PixelRectangles.Reset (
   marshalReset )
import Graphics.Rendering.OpenGL.GL.PixelRectangles.Sink (
   marshalSink, unmarshalSink )
import Graphics.Rendering.OpenGL.GL.StateVar ( StateVar )

--------------------------------------------------------------------------------

#include "HsOpenGLExt.h"

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
   f <- getMinmaxParameteri unmarshalPixelInternalFormat MinmaxFormat
   s <- getMinmaxParameteri unmarshalSink MinmaxSink
   return (f, s)

setMinmax :: (PixelInternalFormat, Sink) -> IO ()
setMinmax (int, sink) =
   glMinmax
      (marshalMinmaxTarget Minmax)
      (marshalPixelInternalFormat' int)
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
