{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.PrimitiveModeInternal
-- Copyright   :  (c) Sven Panne 2002-2013
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This is a purely internal module for (un-)marshaling PrimitiveMode.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.PrimitiveModeInternal (
   marshalPrimitiveMode, unmarshalPrimitiveMode
) where

import Graphics.Rendering.OpenGL.Raw
import Graphics.Rendering.OpenGL.GL.PrimitiveMode

--------------------------------------------------------------------------------

marshalPrimitiveMode :: PrimitiveMode -> GLenum
marshalPrimitiveMode x = case x of
   Points -> gl_POINTS
   Lines -> gl_LINES
   LineLoop -> gl_LINE_LOOP
   LineStrip -> gl_LINE_STRIP
   Triangles -> gl_TRIANGLES
   TriangleStrip -> gl_TRIANGLE_STRIP
   TriangleFan -> gl_TRIANGLE_FAN
   Quads -> gl_QUADS
   QuadStrip -> gl_QUAD_STRIP
   Polygon -> gl_POLYGON
   Patches -> gl_PATCHES

unmarshalPrimitiveMode :: GLenum -> PrimitiveMode
unmarshalPrimitiveMode x
   | x == gl_POINTS = Points
   | x == gl_LINES = Lines
   | x == gl_LINE_LOOP = LineLoop
   | x == gl_LINE_STRIP = LineStrip
   | x == gl_TRIANGLES = Triangles
   | x == gl_TRIANGLE_STRIP = TriangleStrip
   | x == gl_TRIANGLE_FAN = TriangleFan
   | x == gl_QUADS = Quads
   | x == gl_QUAD_STRIP = QuadStrip
   | x == gl_POLYGON = Polygon
   | x == gl_PATCHES = Patches
   | otherwise = error ("unmarshalPrimitiveMode: illegal value " ++ show x)
