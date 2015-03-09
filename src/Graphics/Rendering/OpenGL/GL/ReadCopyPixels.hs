--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.ReadCopyPixels
-- Copyright   :  (c) Sven Panne 2002-2013
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
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
   PixelCopyType(..), copyPixels,

   -- * Copying Pixels for framebuffers
   BlitBuffer(..), blitFramebuffer
) where

import Data.StateVar
import Graphics.Rendering.OpenGL.GL.BufferMode
import Graphics.Rendering.OpenGL.GL.CoordTrans
import Graphics.Rendering.OpenGL.GL.PixelData
import Graphics.Rendering.OpenGL.GL.QueryUtils
import Graphics.Rendering.OpenGL.GL.Texturing.Filter
import Graphics.Rendering.OpenGL.GLU.ErrorsInternal
import Graphics.Rendering.OpenGL.Raw

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

--------------------------------------------------------------------------------

-- | The buffers which can be copied with 'blitFramebuffer'.

data BlitBuffer =
     ColorBuffer'
   | StencilBuffer'
   | DepthBuffer'
   deriving ( Eq, Ord, Show )

marshalBlitBuffer :: BlitBuffer -> GLbitfield
marshalBlitBuffer x = case x of
   ColorBuffer' -> gl_COLOR_BUFFER_BIT
   StencilBuffer' -> gl_STENCIL_BUFFER_BIT
   DepthBuffer' -> gl_DEPTH_BUFFER_BIT

--------------------------------------------------------------------------------

blitFramebuffer :: Position
                -> Position
                -> Position
                -> Position
                -> [BlitBuffer]
                -> TextureFilter
                -> IO ()
blitFramebuffer (Position sx0 sy0)
                (Position sx1 sy1)
                (Position dx0 dy0)
                (Position dx1 dy1)
                buffers
                filt =
   glBlitFramebuffer sx0 sy0 sx1 sy1 dx0 dy0 dx1 dy1
                     (sum (map marshalBlitBuffer buffers))
                     (fromIntegral (marshalMagnificationFilter filt))
