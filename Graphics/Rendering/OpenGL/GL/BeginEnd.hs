--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.BeginEnd
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- This module corresponds to section 2.6 (Begin\/End Paradigm) of the
-- OpenGL 1.4 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.BeginEnd (
   -- * Begin and End Objects
   PrimitiveMode(..),
   renderPrimitive, unsafeRenderPrimitive, primitiveRestart,

   -- * Polygon Edges
   EdgeFlag(..),
   edgeFlag
) where

import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLenum, GLboolean )
import Graphics.Rendering.OpenGL.GL.EdgeFlag (
   EdgeFlag(..), marshalEdgeFlag, unmarshalEdgeFlag )
import Graphics.Rendering.OpenGL.GL.Exception ( finally )
import Graphics.Rendering.OpenGL.GL.Extensions (
   FunPtr, unsafePerformIO, Invoker, getProcAddress )
import Graphics.Rendering.OpenGL.GL.PrimitiveMode (
   PrimitiveMode(..), marshalPrimitiveMode )
import Graphics.Rendering.OpenGL.GL.QueryUtils (
   getBoolean1, GetPName(GetEdgeFlag) )
import Graphics.Rendering.OpenGL.GL.StateVar ( StateVar, makeStateVar )
#ifdef __HADDOCK__
import Graphics.Rendering.OpenGL.GL.Polygons ( PolygonMode(..), polygonMode )
#endif

--------------------------------------------------------------------------------

#include "HsOpenGLExt.h"

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
-- 'Graphics.Rendering.OpenGL.GL.Evaluators.evalPoint1',
-- 'Graphics.Rendering.OpenGL.GL.Evaluators.evalPoint2',
-- 'Graphics.Rendering.OpenGL.GL.Evaluators.evalCoord1',
-- 'Graphics.Rendering.OpenGL.GL.Evaluators.evalCoord1v',
-- 'Graphics.Rendering.OpenGL.GL.Evaluators.evalCoord2',
-- 'Graphics.Rendering.OpenGL.GL.Evaluators.evalCoord2v',
-- 'Graphics.Rendering.OpenGL.GL.Colors.materialAmbient',
-- 'Graphics.Rendering.OpenGL.GL.Colors.materialDiffuse',
-- 'Graphics.Rendering.OpenGL.GL.Colors.materialAmbientAndDiffuse',
-- 'Graphics.Rendering.OpenGL.GL.Colors.materialSpecular',
-- 'Graphics.Rendering.OpenGL.GL.Colors.materialEmission',
-- 'Graphics.Rendering.OpenGL.GL.Colors.materialShininess',
-- 'Graphics.Rendering.OpenGL.GL.DisplayLists.callList',
-- 'Graphics.Rendering.OpenGL.GL.DisplayLists.callLists',
-- and setting 'edgeFlag' are allowed. Writing the respective state variables
-- is allowed in the delimited action, too.
--
-- Regardless of the chosen 'PrimitiveMode', there is no limit to the number of
-- vertices that can be defined during a single 'renderPrimitive'. Lines,
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

renderPrimitive :: PrimitiveMode -> IO a -> IO a
renderPrimitive beginMode action =
   (do glBegin (marshalPrimitiveMode beginMode) ; action) `finally` glEnd

foreign import CALLCONV unsafe "glBegin" glBegin :: GLenum -> IO ()

foreign import CALLCONV unsafe "glEnd" glEnd :: IO ()

-- | A more efficient, but potentially dangerous version of 'renderPrimitive':
-- The given action is not allowed to throw an exception.

unsafeRenderPrimitive :: PrimitiveMode -> IO a -> IO a
unsafeRenderPrimitive beginMode action = do
   glBegin (marshalPrimitiveMode beginMode)
   ret <- action
   glEnd
   return ret

--------------------------------------------------------------------------------

primitiveRestart :: IO ()
primitiveRestart = glPrimitiveRestartNV

EXTENSION_ENTRY("GL_NV_primitive_restart",glPrimitiveRestartNV,IO ())

--------------------------------------------------------------------------------

-- | Each vertex of a polygon, separate triangle, or separate quadrilateral
-- specified during 'renderPrimitive' is marked as the start of either a boundary
-- or nonboundary (interior) edge.
--
-- The vertices of connected triangles and connected quadrilaterals are always
-- marked as boundary, regardless of the value of the edge flag.
--
-- Boundary and nonboundary edge flags on vertices are significant only if
-- 'Graphics.Rendering.OpenGL.GL.Polygons.polygonMode' is set to
-- 'Graphics.Rendering.OpenGL.GL.Polygons.Point' or
-- 'Graphics.Rendering.OpenGL.GL.Polygons.Line'.
--
-- Note that the current edge flag can be updated at any time, in particular
-- during 'renderPrimitive'.

edgeFlag :: StateVar EdgeFlag
edgeFlag =
   makeStateVar (getBoolean1 unmarshalEdgeFlag GetEdgeFlag)
                (glEdgeFlag . marshalEdgeFlag)

foreign import CALLCONV unsafe "glEdgeFlag" glEdgeFlag :: GLboolean -> IO ()
