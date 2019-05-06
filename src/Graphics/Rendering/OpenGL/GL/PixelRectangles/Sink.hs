{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.PixelRectangles.Sink
-- Copyright   :  (c) Sven Panne 2002-2019
-- License     :  BSD3
-- 
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This is a purely internal module for (un-)marshaling Sink.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.PixelRectangles.Sink (
   Sink(..), marshalSink, unmarshalSink
) where

import Graphics.Rendering.OpenGL.GL.GLboolean
import Graphics.GL

--------------------------------------------------------------------------------

data Sink =
     PassThrough
   | Sink
   deriving ( Eq, Ord, Show )

marshalSink :: Sink -> GLboolean
marshalSink x = marshalGLboolean (x == Sink)

unmarshalSink :: GLint -> Sink
unmarshalSink s = if unmarshalGLboolean s then Sink else PassThrough
