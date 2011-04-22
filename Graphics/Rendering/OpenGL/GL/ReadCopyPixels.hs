--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.ReadCopyPixels
-- Copyright   :  (c) Sven Panne 2002-2009
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
--
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to section 4.3 (Drawing, Reading, and Copying Pixels)
-- of the OpenGL 2.1 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.ReadCopyPixels (
   -- * Reading Pixels
   readPixels, readBuffer,

   -- * Copying Pixels
   PixelCopyType(..), copyPixels
) where

import Data.StateVar
import Graphics.Rendering.OpenGL.GL.BufferMode
import Graphics.Rendering.OpenGL.GL.CoordTrans
import Graphics.Rendering.OpenGL.GL.PixelData
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.GLU.ErrorsInternal
import Graphics.Rendering.OpenGL.Raw.ARB.Compatibility ( glCopyPixels )
import Graphics.Rendering.OpenGL.Raw.Core31

--------------------------------------------------------------------------------

readPixels :: Position -> Size -> PixelData a -> IO ()
readPixels (Position x y) (Size w h) pd =
   withPixelData pd $ glReadPixels x y w h

--------------------------------------------------------------------------------

readBuffer :: StateVar BufferMode
readBuffer =
   makeStateVar
      (getEnum1 unmarshalBufferMode GetReadBuffer)
      (maybe recordInvalidValue glReadBuffer . marshalBufferMode)

--------------------------------------------------------------------------------

data PixelCopyType =
     CopyColor
   | CopyDepth
   | CopyStencil
   deriving ( Eq, Ord, Show )

marshalPixelCopyType :: PixelCopyType -> GLenum
marshalPixelCopyType x = case x of
   CopyColor -> gl_COLOR
   CopyDepth -> gl_DEPTH
   CopyStencil -> gl_STENCIL

--------------------------------------------------------------------------------

copyPixels :: Position -> Size -> PixelCopyType -> IO ()
copyPixels (Position x y) (Size w h) t =
   glCopyPixels x y w h (marshalPixelCopyType t)
