{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.BufferMode
-- Copyright   :  (c) Sven Panne 2002-2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This is a purely internal module for (un-)marshaling BufferMode.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.BufferMode (
   BufferMode(..), marshalBufferMode, unmarshalBufferMode,
   unmarshalBufferModeSafe,
   maxColorAttachments,
) where

import Graphics.GL

--------------------------------------------------------------------------------

-- | The set of color buffers which are selected for reading and writing. Note
-- that 'FBOColorAttachment' can only be used with framebuffer objects, while
-- the rest can only be used with the default framebuffer. Furthermore, OpenGL
-- 3.0 deprecated auxiliary buffers, so avoid 'AuxBuffer' in modern code.

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
   | FBOColorAttachment GLsizei
     -- ^ Only the given color attachment of the bound framebufferobject is selected for reading
     -- or writing.
   deriving ( Eq, Ord, Show )

marshalBufferMode :: BufferMode -> Maybe GLenum
marshalBufferMode x = case x of
   NoBuffers -> Just GL_NONE
   FrontLeftBuffer -> Just GL_FRONT_LEFT
   FrontRightBuffer -> Just GL_FRONT_RIGHT
   BackLeftBuffer -> Just GL_BACK_LEFT
   BackRightBuffer -> Just GL_BACK_RIGHT
   FrontBuffers -> Just GL_FRONT
   BackBuffers -> Just GL_BACK
   LeftBuffers -> Just GL_LEFT
   RightBuffers -> Just GL_RIGHT
   FrontAndBackBuffers -> Just GL_FRONT_AND_BACK
   AuxBuffer i
      | fromIntegral i <= maxAuxBuffer -> Just (GL_AUX0 + fromIntegral i)
      | otherwise -> Nothing
   FBOColorAttachment i
      | fromIntegral i <= maxColorAttachments -> Just (GL_COLOR_ATTACHMENT0 + fromIntegral i)
      | otherwise -> Nothing

unmarshalBufferMode :: GLenum -> BufferMode
unmarshalBufferMode x = maybe
   (error ("unmarshalBufferMode: illegal value " ++ show x)) id $ unmarshalBufferModeSafe x

unmarshalBufferModeSafe :: GLenum -> Maybe BufferMode
unmarshalBufferModeSafe x
   | x == GL_NONE = Just NoBuffers
   | x == GL_FRONT_LEFT = Just FrontLeftBuffer
   | x == GL_FRONT_RIGHT = Just FrontRightBuffer
   | x == GL_BACK_LEFT = Just BackLeftBuffer
   | x == GL_BACK_RIGHT = Just BackRightBuffer
   | x == GL_FRONT = Just FrontBuffers
   | x == GL_BACK = Just BackBuffers
   | x == GL_LEFT = Just LeftBuffers
   | x == GL_RIGHT = Just RightBuffers
   | x == GL_FRONT_AND_BACK = Just FrontAndBackBuffers
   | GL_AUX0 <= x && x <= GL_AUX0 + maxAuxBuffer = Just . AuxBuffer . fromIntegral $ x - GL_AUX0
   | GL_COLOR_ATTACHMENT0 <= x && x <= GL_COLOR_ATTACHMENT0 + maxColorAttachments
      = Just . FBOColorAttachment . fromIntegral $ x - GL_COLOR_ATTACHMENT0
   | otherwise = Nothing

maxAuxBuffer :: GLenum
maxAuxBuffer = 246

maxColorAttachments :: GLenum
maxColorAttachments = 16
