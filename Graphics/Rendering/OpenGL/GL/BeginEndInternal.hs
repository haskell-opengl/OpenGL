-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.BeginEndInternal
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- This is a purely internal module related to section 2.6 (Begin\/End Paradigm)
-- of the OpenGL 1.4 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.BeginEndInternal (
   PrimitiveMode(..), marshalPrimitiveMode, unmarshalPrimitiveMode,
   EdgeFlag(..), marshalEdgeFlag, unmarshalEdgeFlag
) where

import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLenum )
import Graphics.Rendering.OpenGL.GL.GLboolean (
   GLboolean, marshalGLboolean, unmarshalGLboolean )

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
   Points -> 0x0
   Lines -> 0x1
   LineLoop -> 0x2
   LineStrip -> 0x3
   Triangles -> 0x4
   TriangleStrip -> 0x5
   TriangleFan -> 0x6
   Quads -> 0x7
   QuadStrip -> 0x8
   Polygon -> 0x9

unmarshalPrimitiveMode :: GLenum -> PrimitiveMode
unmarshalPrimitiveMode x
   | x == 0x0 = Points
   | x == 0x1 = Lines
   | x == 0x2 = LineLoop
   | x == 0x3 = LineStrip
   | x == 0x4 = Triangles
   | x == 0x5 = TriangleStrip
   | x == 0x6 = TriangleFan
   | x == 0x7 = Quads
   | x == 0x8 = QuadStrip
   | x == 0x9 = Polygon
   | otherwise = error ("unmarshalPrimitiveMode: illegal value " ++ show x)

--------------------------------------------------------------------------------

-- | A vertex can begin an edge which lies in the interior of its polygon or on
-- the polygon\'s boundary.

data EdgeFlag = BeginsInteriorEdge | BeginsBoundaryEdge
   deriving ( Eq, Ord, Show )

marshalEdgeFlag :: EdgeFlag -> GLboolean
marshalEdgeFlag BeginsInteriorEdge = marshalGLboolean False
marshalEdgeFlag BeginsBoundaryEdge = marshalGLboolean True

unmarshalEdgeFlag :: GLboolean -> EdgeFlag
unmarshalEdgeFlag f
   | unmarshalGLboolean f = BeginsBoundaryEdge
   | otherwise            = BeginsInteriorEdge
