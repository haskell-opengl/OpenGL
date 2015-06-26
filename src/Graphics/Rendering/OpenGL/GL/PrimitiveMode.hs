--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.PrimitiveMode
-- Copyright   :  (c) Sven Panne 2002-2013, Tobias Markus 2015
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to section 10.1 (Primitive Types) of the OpenGL 4.4
-- specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.PrimitiveMode (
   -- * Primitive Modes
   PrimitiveMode(..),
   -- * Patches (Tessellation)
   patchVertices, maxPatchVertices,
   patchDefaultOuterLevel, patchDefaultInnerLevel, maxTessGenLevel
) where

import Data.StateVar
import Foreign.Marshal.Array
import Graphics.Rendering.OpenGL.GL.PeekPoke
import Graphics.Rendering.OpenGL.GL.QueryUtils.PName
import Graphics.Rendering.OpenGL.Raw

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
   | Patches
     -- ^ Only used in conjunction with tessellation. The number of vertices per
     -- patch can be set with 'patchVertices'.
   deriving ( Eq, Ord, Show )

-- | 'patchVertices' is the number of vertices per patch primitive.
--
-- An 'Graphics.Rendering.OpenGL.GLU.Errors.InvalidValue' is generated if
-- 'patchVertices' is set to a value less than or equal to zero or greater
-- than the implementation-dependent maximum value 'maxPatchVertices'.

patchVertices :: StateVar GLsizei
patchVertices =
  makeStateVar (getSizei1 id GetPatchVertices)
               (glPatchParameteri gl_PATCH_VERTICES . fromIntegral)

-- | Contains the maximumum number of vertices in a single patch.

maxPatchVertices :: GettableStateVar GLsizei
maxPatchVertices = makeGettableStateVar $ getSizei1 id GetMaxPatchVertices

-- | Contains the four default outer tessellation levels to be used when no
-- tessellation control shader is present.

patchDefaultOuterLevel :: StateVar (GLfloat, GLfloat, GLfloat, GLfloat)
patchDefaultOuterLevel =
  makeStateVar
    (getFloat4 (,,,) GetPatchDefaultOuterLevel)
    (\(l0, l1, l2, l3) -> allocaArray 4 $ \ptr -> do
                            poke4 ptr l0 l1 l2 l3
                            glPatchParameterfv gl_PATCH_DEFAULT_OUTER_LEVEL ptr)

-- | Contains the two default inner tessellation levels to be used when no
-- tessellation control shader is present.

patchDefaultInnerLevel :: StateVar (GLfloat, GLfloat)
patchDefaultInnerLevel =
  makeStateVar
    (getFloat2 (,) GetPatchDefaultInnerLevel)
    (\(l0, l1) -> allocaArray 2 $ \ptr -> do
                    poke2 ptr l0 l1
                    glPatchParameterfv gl_PATCH_DEFAULT_INNER_LEVEL ptr)

-- | Contains the maximum allowed tessellation level.

maxTessGenLevel :: GettableStateVar GLsizei
maxTessGenLevel = makeGettableStateVar $ getSizei1 id GetMaxTessGenLevel
