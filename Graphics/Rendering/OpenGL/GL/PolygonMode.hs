-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.PolygonMode
-- Copyright   :  (c) Sven Panne 2002-2009
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  stable
-- Portability :  portable
--
-- This is a purely internal module for (un-)marshaling PolygonMode.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.PolygonMode (
   PolygonMode(..), marshalPolygonMode, unmarshalPolygonMode
) where

import Graphics.Rendering.OpenGL.Raw.Core31

--------------------------------------------------------------------------------

data PolygonMode =
     Point
   | Line
   | Fill
   deriving ( Eq, Ord, Show )

marshalPolygonMode :: PolygonMode -> GLenum
marshalPolygonMode x = case x of
   Point -> gl_POINT
   Line -> gl_LINE
   Fill -> gl_FILL

unmarshalPolygonMode :: GLenum -> PolygonMode
unmarshalPolygonMode x
   | x == gl_POINT = Point
   | x == gl_LINE = Line
   | x == gl_FILL = Fill
   | otherwise = error ("unmarshalPolygonMode: illegal value " ++ show x)
