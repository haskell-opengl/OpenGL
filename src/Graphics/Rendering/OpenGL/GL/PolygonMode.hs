{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.PolygonMode
-- Copyright   :  (c) Sven Panne 2002-2019
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

import Graphics.GL

--------------------------------------------------------------------------------

data PolygonMode =
     Point
   | Line
   | Fill
   deriving ( Eq, Ord, Show )

marshalPolygonMode :: PolygonMode -> GLenum
marshalPolygonMode x = case x of
   Point -> GL_POINT
   Line -> GL_LINE
   Fill -> GL_FILL

unmarshalPolygonMode :: GLenum -> PolygonMode
unmarshalPolygonMode x
   | x == GL_POINT = Point
   | x == GL_LINE = Line
   | x == GL_FILL = Fill
   | otherwise = error ("unmarshalPolygonMode: illegal value " ++ show x)
