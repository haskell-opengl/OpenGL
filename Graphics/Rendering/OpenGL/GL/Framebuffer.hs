--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Framebuffer
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  experimental
-- Portability :  portable
--
-- This module corresponds to section 4.2 (Whole Framebuffer Operations) of the
-- OpenGL 1.4 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Framebuffer (
   -- * Querying the Buffer Configuration
   auxBuffers, doubleBuffer, stereo,

   -- * Selecting a Buffer for Writing
   DrawBufferMode(..), drawBuffer
) where

import Graphics.Rendering.OpenGL.GL.BasicTypes (
   unmarshalGLboolean, GLenum, GLsizei )
import Graphics.Rendering.OpenGL.GL.Query (
   GetPName(GetAuxBuffers,GetDoublebuffer,GetStereo,GetDrawBuffer),
   getInteger1, getBoolean1 )
import Graphics.Rendering.OpenGL.GL.StateVar (
   GettableStateVar, makeGettableStateVar, StateVar, makeStateVar )

--------------------------------------------------------------------------------

-- | The implementation and context dependent number of auxiliary buffers.

auxBuffers :: GettableStateVar GLsizei
auxBuffers =
   makeGettableStateVar (getInteger1 fromIntegral GetAuxBuffers)

-- | 'True' if front and back buffers exist.

doubleBuffer :: GettableStateVar Bool
doubleBuffer =
   makeGettableStateVar (getBoolean1 unmarshalGLboolean GetDoublebuffer)

-- | 'True' if left and right buffers exist.

stereo :: GettableStateVar Bool
stereo =
   makeGettableStateVar (getBoolean1 unmarshalGLboolean GetStereo)

--------------------------------------------------------------------------------

-- | The buffers into which colors are written.

data DrawBufferMode =
     DrawBufferNone
     -- ^ No color buffers are written.
   | DrawBufferFrontLeft
     -- ^ Only the front left color buffer is written.
   | DrawBufferFrontRight
     -- ^ Only the front right color buffer is written.
   | DrawBufferBackLeft
     -- ^ Only the  back left color buffer is written.
   | DrawBufferBackRight
     -- ^ Only the back right color buffer is written.
   | DrawBufferFront
     -- ^ Only the front left and front right color buffers are written. If
     -- there is no front right color buffer, only the front left color buffer
     -- is written.
   | DrawBufferBack
     -- ^ Only the back left and back right color buffers are written. If there
     -- is no back right color buffer, only the back left color buffer is
     -- written.
   | DrawBufferLeft
     -- ^ Only the front left and back left color buffers are written. If there
     -- is no back left color buffer, only the front left color buffer is
     -- written.
   | DrawBufferRight
     -- ^ Only the front right and back right color buffers are written. If
     -- there is no back right color buffer, only the front right color buffer
     -- is written.
   | DrawBufferFrontAndBack
     -- ^ All the front and back color buffers (front left, front right, back
     -- left, back right) are written. If there are no back color buffers, only
     -- the front left and front right color buffers are written. If there are
     -- no right color buffers, only the front left and back left color buffers
     -- are written. If there are no right or back color buffers, only the front
     -- left color buffer is written.
   | DrawBufferAux GLsizei
     -- ^ Only the givem auxiliary color buffer no. /i/ is written.
   deriving ( Eq, Ord, Show )

marshalDrawBufferMode :: DrawBufferMode -> GLenum
marshalDrawBufferMode x = case x of
   DrawBufferNone -> 0x0
   DrawBufferFrontLeft -> 0x400
   DrawBufferFrontRight -> 0x401
   DrawBufferBackLeft -> 0x402
   DrawBufferBackRight -> 0x403
   DrawBufferFront -> 0x404
   DrawBufferBack -> 0x405
   DrawBufferLeft -> 0x406
   DrawBufferRight -> 0x407
   DrawBufferFrontAndBack -> 0x408
   DrawBufferAux i -> 0x409 + fromIntegral i

unmarshalDrawBufferMode :: GLenum -> DrawBufferMode
unmarshalDrawBufferMode x
   | x == 0x0 = DrawBufferNone
   | x == 0x400 = DrawBufferFrontLeft
   | x == 0x401 = DrawBufferFrontRight
   | x == 0x402 = DrawBufferBackLeft
   | x == 0x403 = DrawBufferBackRight
   | x == 0x404 = DrawBufferFront
   | x == 0x405 = DrawBufferBack
   | x == 0x406 = DrawBufferLeft
   | x == 0x407 = DrawBufferRight
   | x == 0x408 = DrawBufferFrontAndBack
   | x >= 0x409 = DrawBufferAux (fromIntegral x - 0x409)
   | otherwise = error ("unmarshalDrawBufferMode: illegal value " ++ show x)

--------------------------------------------------------------------------------


-- | When colors are written to the framebuffer, they are written into the color
-- buffers specified by 'drawBuffer'.
-- 
-- If more than one color buffer is selected for drawing, then blending or
-- logical operations are computed and applied independently for each color
-- buffer and can produce different results in each buffer.
--
-- Monoscopic contexts include only left buffers, and stereoscopic contexts
-- include both left and right buffers. Likewise, single-buffered contexts
-- include only front buffers, and double-buffered contexts include both front
-- and back buffers. The context is selected at GL initialization.
--
-- The initial value is 'DrawBufferFront' for single-buffered contexts, and
-- 'DrawBufferBack' for double-buffered contexts.

drawBuffer :: StateVar DrawBufferMode
drawBuffer =
   makeStateVar
      (getInteger1 (unmarshalDrawBufferMode . fromIntegral) GetDrawBuffer)
      (glDrawBuffer . marshalDrawBufferMode)

foreign import CALLCONV unsafe "glDrawBuffer" glDrawBuffer :: GLenum -> IO ()
