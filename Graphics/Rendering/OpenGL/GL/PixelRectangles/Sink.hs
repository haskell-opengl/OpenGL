-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.PixelRectangles.Sink
-- Copyright   :  (c) Sven Panne 2002-2005
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
-- This is a purely internal module for (un-)marshaling Sink.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.PixelRectangles.Sink (
   Sink(..), marshalSink, unmarshalSink
) where

import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLboolean, GLint )
import Graphics.Rendering.OpenGL.GL.GLboolean (
   marshalGLboolean, unmarshalGLboolean )

--------------------------------------------------------------------------------

data Sink =
     PassThrough
   | Sink
   deriving ( Eq, Ord, Show )

marshalSink :: Sink -> GLboolean
marshalSink x = marshalGLboolean (x == Sink)

unmarshalSink :: GLint -> Sink
unmarshalSink s = if unmarshalGLboolean s then Sink else PassThrough
