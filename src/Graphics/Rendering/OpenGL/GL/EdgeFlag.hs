{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.EdgeFlag
-- Copyright   :  (c) Sven Panne 2002-2019
-- License     :  BSD3
-- 
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This is a purely internal module for (un-)marshaling EdgeFlag.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.EdgeFlag (
   EdgeFlag(..), marshalEdgeFlag, unmarshalEdgeFlag
) where

import Graphics.Rendering.OpenGL.GL.GLboolean
import Graphics.GL

--------------------------------------------------------------------------------

-- | A vertex can begin an edge which lies in the interior of its polygon or on
-- the polygon\'s boundary.

data EdgeFlag = BeginsInteriorEdge | BeginsBoundaryEdge
   deriving ( Eq, Ord, Show )

marshalEdgeFlag :: EdgeFlag -> GLboolean
marshalEdgeFlag = marshalGLboolean . (BeginsBoundaryEdge ==)

unmarshalEdgeFlag :: GLboolean -> EdgeFlag
unmarshalEdgeFlag f =
   if unmarshalGLboolean f then BeginsBoundaryEdge else BeginsInteriorEdge
