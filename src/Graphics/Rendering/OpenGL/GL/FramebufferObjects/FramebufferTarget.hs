{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.FramebufferObjects.FramebufferTarget
-- Copyright   :  (c) Sven Panne 2013-2016
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This is a purely internal module for marshaling FramebufferTargets.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.FramebufferObjects.FramebufferTarget (
   FramebufferTarget(..), marshalFramebufferTarget
) where

import Graphics.GL

--------------------------------------------------------------------------------

data FramebufferTarget =
     DrawFramebuffer
   | ReadFramebuffer
   | Framebuffer
   deriving ( Eq, Ord, Show )

marshalFramebufferTarget :: FramebufferTarget -> GLenum
marshalFramebufferTarget xs = case xs of
   DrawFramebuffer -> gl_DRAW_FRAMEBUFFER
   ReadFramebuffer -> gl_READ_FRAMEBUFFER
   Framebuffer -> gl_FRAMEBUFFER
