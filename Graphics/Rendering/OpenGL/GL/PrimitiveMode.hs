-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.PrimitiveMode
-- Copyright   :  (c) Sven Panne 2002-2009
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  stable
-- Portability :  portable
--
-- This is a purely internal module for (un-)marshaling PrimitiveMode.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.PrimitiveMode (
   PrimitiveMode(..), marshalPrimitiveMode, unmarshalPrimitiveMode
) where

import Graphics.Rendering.OpenGL.Raw.Core31
import Graphics.Rendering.OpenGL.Raw.ARB.Compatibility (
   gl_QUADS, gl_QUAD_STRIP, gl_POLYGON )

--------------------------------------------------------------------------------

-- | Specification of the way the vertices given during 'renderPrimitive' are
-- interpreted. In the description of the constructors, /n/ is an integer count
-- starting at one, and /N/ is the total number of vertices specified.

data PrimitiveMode =
     Points
     -- ^ Treats each vertex as a single point. Vertex /n/ defines point /n/.
     -- /N/ points are drawn.
   | Lines
     -- ^ Treats each pair of vertices as an independent line segment. Vertices
     -- 2/n/-1 and 2/n/ define line /n/. /N/\/2 lines are drawn.
   | LineLoop
     -- ^ Draws a connected group of line segments from the first vertex to the
     -- last, then back to the first. Vertices /n/ and /n/+1 define line /n/.
     -- The last line, however, is defined by vertices /N/ and 1. /N/ lines
     -- are drawn.
   | LineStrip
     -- ^ Draws a connected group of line  segments from the first vertex to the
     -- last. Vertices /n/ and /n/+1 define line /n/. /N/-1 lines are drawn.
   | Triangles
     -- ^ Treats each triplet of vertices as an independent triangle. Vertices
     -- /3n-2/, /3n-1/, and /3n/ define triangle /n/. /N\/3/ triangles are drawn.
   | TriangleStrip
     -- ^ Draws a connected group of triangles. One triangle is defined for each
     -- vertex presented after the first two vertices. For odd /n/, vertices
     -- /n/, /n/+1, and /n/+2 define triangle /n/. For even /n/, vertices /n/+1,
     -- /n/, and /n/+2 define triangle /n/. /N/-2 triangles are drawn.
   | TriangleFan
     -- ^ Draws a connected group of triangles. One triangle is defined for each
     -- vertex presented after the first two vertices. Vertices 1, /n/+1, and
     -- /n/+2 define triangle /n/. /N/-2 triangles are drawn.
   | Quads
     -- ^ Treats each group of four vertices as an independent quadrilateral.
     -- Vertices 4/n/-3, 4/n/-2, 4/n/-1, and 4/n/ define quadrilateral /n/.
     -- /N/\/4 quadrilaterals are drawn.
   | QuadStrip
     -- ^ Draws a connected group of quadrilaterals. One quadrilateral is
     --defined for each pair of vertices presented after the first pair.
     -- Vertices 2/n/-1, 2/n/, 2/n/+2, and 2/n/+1 define quadrilateral /n/.
     -- /N/\/2-1 quadrilaterals are drawn. Note that the order in which vertices
     -- are used to construct a quadrilateral from strip data is different from
     -- that used with independent data.
   | Polygon
     -- ^ Draws a single, convex polygon. Vertices 1 through /N/ define this
     -- polygon.
   deriving ( Eq, Ord, Show )

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
   | otherwise = error ("unmarshalPrimitiveMode: illegal value " ++ show x)
