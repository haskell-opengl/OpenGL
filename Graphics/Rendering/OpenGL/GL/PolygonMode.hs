-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.PolygonMode
-- Copyright   :  (c) Sven Panne 2003
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven_panne@yahoo.com
-- Stability   :  provisional
-- Portability :  portable
--
-- This is a purely internal module for (un-)marshaling PolygonMode.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.PolygonMode (
   PolygonMode(..), marshalPolygonMode, unmarshalPolygonMode
) where

import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLenum )

--------------------------------------------------------------------------------

data PolygonMode =
     Point
   | Line
   | Fill
   deriving ( Eq, Ord, Show )

marshalPolygonMode :: PolygonMode -> GLenum
marshalPolygonMode x = case x of
   Point -> 0x1b00
   Line -> 0x1b01
   Fill -> 0x1b02

unmarshalPolygonMode :: GLenum -> PolygonMode
unmarshalPolygonMode x
   | x == 0x1b00 = Point
   | x == 0x1b01 = Line
   | x == 0x1b02 = Fill
   | otherwise = error ("unmarshalPolygonMode: illegal value " ++ show x)
