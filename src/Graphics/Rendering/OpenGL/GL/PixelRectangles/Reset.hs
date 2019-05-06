{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.PixelRectangles.Reset
-- Copyright   :  (c) Sven Panne 2002-2019
-- License     :  BSD3
-- 
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This is a purely internal module for (un-)marshaling Reset.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.PixelRectangles.Reset (
   Reset(..), marshalReset
) where

import Graphics.Rendering.OpenGL.GL.GLboolean
import Graphics.GL

--------------------------------------------------------------------------------

data Reset =
     NoReset
   | Reset
   deriving ( Eq, Ord, Show )

marshalReset :: Reset -> GLboolean
marshalReset x = marshalGLboolean (x == Reset)
