-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.EdgeFlag
-- Copyright   :  (c) Sven Panne 2002-2005
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
-- This is a purely internal module for (un-)marshaling EdgeFlag.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.EdgeFlag (
   EdgeFlag(..), marshalEdgeFlag, unmarshalEdgeFlag
) where

import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLboolean )
import Graphics.Rendering.OpenGL.GL.GLboolean (
   marshalGLboolean, unmarshalGLboolean )

--------------------------------------------------------------------------------

-- | A vertex can begin an edge which lies in the interior of its polygon or on
-- the polygon\'s boundary.

data EdgeFlag = BeginsInteriorEdge | BeginsBoundaryEdge
   deriving ( Eq, Ord, Show )

marshalEdgeFlag :: EdgeFlag -> GLboolean
marshalEdgeFlag = marshalGLboolean . (BeginsBoundaryEdge ==)

unmarshalEdgeFlag :: GLboolean -> EdgeFlag
unmarshalEdgeFlag f =
   if unmarshalGLboolean f then BeginsBoundaryEdge else BeginsBoundaryEdge
