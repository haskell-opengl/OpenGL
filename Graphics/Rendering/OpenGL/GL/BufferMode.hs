-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.BufferMode
-- Copyright   :  (c) Sven Panne 2002-2005
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
-- This is a purely internal module for (un-)marshaling BufferMode.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.BufferMode (
   BufferMode(..), marshalBufferMode, unmarshalBufferMode
) where

import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLenum, GLsizei )

--------------------------------------------------------------------------------

-- | The set of color buffers which are selected for reading and writing.

data BufferMode =
     NoBuffers
     -- ^ No color buffers are selected.
   | FrontLeftBuffer
     -- ^ Only the front left color buffer is selected.
   | FrontRightBuffer
     -- ^ Only the front right color buffer is selected.
   | BackLeftBuffer
     -- ^ Only the  back left color buffer is selected.
   | BackRightBuffer
     -- ^ Only the back right color buffer is selected.
   | FrontBuffers
     -- ^ Only the front left and front right color buffers are selected. If
     -- there is no front right color buffer, only the front left color buffer
     -- is selected.
   | BackBuffers
     -- ^ Only the back left and back right color buffers are selected. If there
     -- is no back right color buffer, only the back left color buffer is
     -- selected.
   | LeftBuffers
     -- ^ Only the front left and back left color buffers are selected. If there
     -- is no back left color buffer, only the front left color buffer is
     -- selected.
   | RightBuffers
     -- ^ Only the front right and back right color buffers are selected. If
     -- there is no back right color buffer, only the front right color buffer
     -- is selected.
   | FrontAndBackBuffers
     -- ^ All the front and back color buffers (front left, front right, back
     -- left, back right) are selected. If there are no back color buffers, only
     -- the front left and front right color buffers are selected. If there are
     -- no right color buffers, only the front left and back left color buffers
     -- are selected. If there are no right or back color buffers, only the
     -- front left color buffer is selected.
   | AuxBuffer GLsizei
     -- ^ Only the given auxiliary color buffer no. /i/ is selected.
   deriving ( Eq, Ord, Show )

marshalBufferMode :: BufferMode -> Maybe GLenum
marshalBufferMode x = case x of
   NoBuffers -> Just 0x0
   FrontLeftBuffer -> Just 0x400
   FrontRightBuffer -> Just 0x401
   BackLeftBuffer -> Just 0x402
   BackRightBuffer -> Just 0x403
   FrontBuffers -> Just 0x404
   BackBuffers -> Just 0x405
   LeftBuffers -> Just 0x406
   RightBuffers -> Just 0x407
   FrontAndBackBuffers -> Just 0x408
   AuxBuffer i
      | i <= 246  -> Just (0x409 + fromIntegral i)
      | otherwise -> Nothing

unmarshalBufferMode :: GLenum -> BufferMode
unmarshalBufferMode x
   | x == 0x0 = NoBuffers
   | x == 0x400 = FrontLeftBuffer
   | x == 0x401 = FrontRightBuffer
   | x == 0x402 = BackLeftBuffer
   | x == 0x403 = BackRightBuffer
   | x == 0x404 = FrontBuffers
   | x == 0x405 = BackBuffers
   | x == 0x406 = LeftBuffers
   | x == 0x407 = RightBuffers
   | x == 0x408 = FrontAndBackBuffers
   | 0x409 <= x && x <= 0x4ff = AuxBuffer (fromIntegral x - 0x409)
   | otherwise = error ("unmarshalBufferMode: illegal value " ++ show x)
