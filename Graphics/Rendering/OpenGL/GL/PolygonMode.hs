{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.PolygonMode
-- Copyright   :  (c) Sven Panne 2002-2013
-- License     :  BSD3
-- 
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This is a purely internal module for (un-)marshaling PolygonMode.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.PolygonMode (
   PolygonMode(..), marshalPolygonMode, unmarshalPolygonMode
) where

import Graphics.Rendering.OpenGL.Raw

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
