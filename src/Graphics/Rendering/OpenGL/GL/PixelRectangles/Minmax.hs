--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.PixelRectangles.Minmax
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

module Graphics.Rendering.OpenGL.GL.PixelRectangles.Minmax (
   minmax, getMinmax, resetMinmax
) where

import Data.StateVar
import Foreign.Marshal.Utils
import Graphics.Rendering.OpenGL.GL.Capability
import Graphics.Rendering.OpenGL.GL.PeekPoke
import Graphics.Rendering.OpenGL.GL.PixelData
import Graphics.Rendering.OpenGL.GL.PixelRectangles.Reset
import Graphics.Rendering.OpenGL.GL.PixelRectangles.Sink
import Graphics.Rendering.OpenGL.GL.Texturing.PixelInternalFormat
import Graphics.Rendering.OpenGL.Raw

--------------------------------------------------------------------------------

data MinmaxTarget =
     Minmax

marshalMinmaxTarget :: MinmaxTarget -> GLenum
marshalMinmaxTarget x = case x of
   Minmax -> gl_MINMAX

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

--------------------------------------------------------------------------------

getMinmax :: Reset -> PixelData a -> IO ()
getMinmax reset pd =
   withPixelData pd $
      glGetMinmax (marshalMinmaxTarget Minmax) (marshalReset reset)

--------------------------------------------------------------------------------

resetMinmax :: IO ()
resetMinmax = glResetMinmax (marshalMinmaxTarget Minmax)

--------------------------------------------------------------------------------

data GetMinmaxParameterPName =
     MinmaxFormat
   | MinmaxSink

marshalGetMinmaxParameterPName :: GetMinmaxParameterPName -> GLenum
marshalGetMinmaxParameterPName x = case x of
   MinmaxFormat -> gl_MINMAX_FORMAT
   MinmaxSink -> gl_MINMAX_SINK

--------------------------------------------------------------------------------

getMinmaxParameteri :: (GLint -> a) -> GetMinmaxParameterPName -> IO a
getMinmaxParameteri f p =
   with 0 $ \buf -> do
      glGetMinmaxParameteriv
         (marshalMinmaxTarget Minmax)
         (marshalGetMinmaxParameterPName p)
         buf
      peek1 f buf
