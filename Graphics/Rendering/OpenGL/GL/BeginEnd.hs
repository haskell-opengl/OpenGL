-- #prune
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.BeginEnd
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  experimental
-- Portability :  portable
--
-- This module corresponds to section 2.6 (Begin\/End Paradigm) of the
-- OpenGL 1.4 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.BeginEnd (
   -- * Begin and End Objects
   BeginMode(..),
   unmarshalBeginMode,   -- used only internally
   withBeginMode,

   -- * Polygon Edges
   edgeFlag, edgeFlagv
   
) where

import Foreign.Ptr ( Ptr )
import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLboolean, GLenum )

--------------------------------------------------------------------------------

-- | Specification of the way the vertices given during 'withBeginMode' are
-- interpreted. In the description of the constructors, /n/ is an integer count
-- starting at one, and /N/ is the total number of vertices specified.

data BeginMode =
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

marshalBeginMode :: BeginMode -> GLenum
marshalBeginMode x = case x of
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

unmarshalBeginMode :: GLenum -> BeginMode
unmarshalBeginMode x
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
   | otherwise = error ("unmarshalBeginMode: illegal value " ++ show x)

--------------------------------------------------------------------------------

-- | Delimit the vertices that define a primitive or a group of like primitives.
--
-- Only a subset of GL commands can be used in the delimited action:
-- Those for specifying vertex coordinates
-- ('Graphics.Rendering.OpenGL.GL.VertexSpec.vertex',
--  'Graphics.Rendering.OpenGL.GL.VertexSpec.vertexv'),
-- vertex colors
-- ('Graphics.Rendering.OpenGL.GL.VertexSpec.color',
--  'Graphics.Rendering.OpenGL.GL.VertexSpec.colorv',
--  'Graphics.Rendering.OpenGL.GL.VertexSpec.secondaryColor',
--  'Graphics.Rendering.OpenGL.GL.VertexSpec.secondaryColorv',
--  'Graphics.Rendering.OpenGL.GL.VertexSpec.index',
--  'Graphics.Rendering.OpenGL.GL.VertexSpec.indexv'),
-- normal
-- ('Graphics.Rendering.OpenGL.GL.VertexSpec.normal',
--  'Graphics.Rendering.OpenGL.GL.VertexSpec.normalv'),
-- texture coordinates
-- ('Graphics.Rendering.OpenGL.GL.VertexSpec.texCoord',
--  'Graphics.Rendering.OpenGL.GL.VertexSpec.texCoordv',
--  'Graphics.Rendering.OpenGL.GL.VertexSpec.multiTexCoord',
--  'Graphics.Rendering.OpenGL.GL.VertexSpec.multiTexCoordv'),
-- and fog coordinates
-- ('Graphics.Rendering.OpenGL.GL.VertexSpec.fogCoord',
--  'Graphics.Rendering.OpenGL.GL.VertexSpec.fogCoordv').
-- Additionally,
-- 'Graphics.Rendering.OpenGL.GL.ToDo.evalCoord',
-- 'Graphics.Rendering.OpenGL.GL.ToDo.evalPoint',
-- 'Graphics.Rendering.OpenGL.GL.ToDo.material',
-- 'Graphics.Rendering.OpenGL.GL.ToDo.callList',
-- 'edgeFlag', and 'edgeFlagv' are allowed.
--
-- Regardless of the chosen 'BeginMode', there is no limit to the number of
-- vertices that can be defined during a single 'withBeginMode'. Lines,
-- triangles, quadrilaterals, and polygons that are incompletely specified are
-- not drawn. Incomplete specification results when either too few vertices are
-- provided to specify even a single primitive or when an incorrect multiple of
-- vertices is specified. The incomplete primitive is ignored; the rest are
-- drawn.
--
-- The minimum specification of vertices for each primitive is as follows: 1
-- for a point, 2 for a line, 3 for a triangle, 4 for a quadrilateral, and 3 for
-- a polygon. Modes that require a certain multiple of vertices are 'Lines' (2),
-- 'Triangles' (3), 'Quads' (4), and 'QuadStrip' (2).

withBeginMode :: BeginMode -> IO a -> IO a
withBeginMode beginMode action = do
   -- ToDo: Should we use bracket here or is it too costly?
   glBegin (marshalBeginMode beginMode)
   val <- action
   glEnd
   return val

foreign import CALLCONV unsafe "glBegin" glBegin :: GLenum -> IO ()

foreign import CALLCONV unsafe "glEnd" glEnd :: IO ()

--------------------------------------------------------------------------------

-- | Flag edges as either boundary or nonboundary.
--
-- Each vertex of a polygon, separate triangle, or separate quadrilateral
-- specified during 'withBeginMode' is marked as the start of either a boundary
-- or nonboundary edge. If the current edge flag is 'True' when the vertex is
-- specified, the vertex is marked as the start of a boundary edge. Otherwise,
-- the vertex is marked as the start of a nonboundary edge. 'edgeFlag' sets the
-- edge flag bit to the given value.
--
-- The vertices of connected triangles and connected quadrilaterals are always
-- marked as boundary, regardless of the value of the edge flag.
--
-- Boundary and nonboundary edge flags on vertices are significant only if
-- 'Graphics.Rendering.OpenGL.GL.ToDo.polygonMode' is set to
-- 'Graphics.Rendering.OpenGL.GL.ToDo.Point' or
-- 'Graphics.Rendering.OpenGL.GL.ToDo.Line'.
--
-- Note that the current edge flag can be updated at any time. In particular,
-- 'edgeFlag' can be called during 'withBeginMode'.

foreign import CALLCONV unsafe "glEdgeFlag" edgeFlag :: GLboolean -> IO ()

-- | A pointer variant of 'edgeFlag'. Use with care.

foreign import CALLCONV unsafe "glEdgeFlagv" edgeFlagv :: Ptr GLboolean -> IO ()
