--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.ConditionalRendering
-- Copyright   :  (c) Sven Panne 2019
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This module corresponds to section 10.10 (Conditional Rendering) of the
-- OpenGL 4.4 specs.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.ConditionalRendering (
   ConditionalRenderMode(..),
   beginConditionalRender, endConditionalRender, withConditionalRender
) where

import Graphics.Rendering.OpenGL.GL.Exception
import Graphics.Rendering.OpenGL.GL.QueryObject
import Graphics.GL

--------------------------------------------------------------------------------

data ConditionalRenderMode =
     QueryWait
   | QueryNoWait
   | QueryByRegionWait
   | QueryByRegionNoWait
   deriving ( Eq, Ord, Show )

marshalConditionalRenderMode :: ConditionalRenderMode -> GLenum
marshalConditionalRenderMode x = case x of
   QueryWait -> GL_QUERY_WAIT
   QueryNoWait -> GL_QUERY_NO_WAIT
   QueryByRegionWait -> GL_QUERY_BY_REGION_WAIT
   QueryByRegionNoWait -> GL_QUERY_BY_REGION_NO_WAIT

--------------------------------------------------------------------------------

beginConditionalRender :: QueryObject -> ConditionalRenderMode -> IO ()
beginConditionalRender q =
   glBeginConditionalRender (queryID q) . marshalConditionalRenderMode

endConditionalRender :: IO ()
endConditionalRender = glEndConditionalRender

withConditionalRender :: QueryObject -> ConditionalRenderMode -> IO a -> IO a
withConditionalRender q m =
   bracket_ (beginConditionalRender q m) endConditionalRender
