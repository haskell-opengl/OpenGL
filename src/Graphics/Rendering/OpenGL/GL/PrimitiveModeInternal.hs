{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.PrimitiveModeInternal
-- Copyright   :  (c) Sven Panne 2002-2019
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

import Graphics.GL
import Graphics.Rendering.OpenGL.GL.PrimitiveMode

--------------------------------------------------------------------------------

marshalPrimitiveMode :: PrimitiveMode -> GLenum
marshalPrimitiveMode x = case x of
   Points -> GL_POINTS
   Lines -> GL_LINES
   LineLoop -> GL_LINE_LOOP
   LineStrip -> GL_LINE_STRIP
   Triangles -> GL_TRIANGLES
   TriangleStrip -> GL_TRIANGLE_STRIP
   TriangleFan -> GL_TRIANGLE_FAN
   Quads -> GL_QUADS
   QuadStrip -> GL_QUAD_STRIP
   Polygon -> GL_POLYGON
   Patches -> GL_PATCHES

unmarshalPrimitiveMode :: GLenum -> PrimitiveMode
unmarshalPrimitiveMode x
   | x == GL_POINTS = Points
   | x == GL_LINES = Lines
   | x == GL_LINE_LOOP = LineLoop
   | x == GL_LINE_STRIP = LineStrip
   | x == GL_TRIANGLES = Triangles
   | x == GL_TRIANGLE_STRIP = TriangleStrip
   | x == GL_TRIANGLE_FAN = TriangleFan
   | x == GL_QUADS = Quads
   | x == GL_QUAD_STRIP = QuadStrip
   | x == GL_POLYGON = Polygon
   | x == GL_PATCHES = Patches
   | otherwise = error ("unmarshalPrimitiveMode: illegal value " ++ show x)
