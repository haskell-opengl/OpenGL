-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.ComparisonFunction
-- Copyright   :  (c) Sven Panne 2002-2009
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  stable
-- Portability :  portable
--
-- This is a purely internal module for (un-)marshaling ComparisonFunction.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.ComparisonFunction (
   ComparisonFunction(..), marshalComparisonFunction,
   unmarshalComparisonFunction
) where

import Graphics.Rendering.OpenGL.Raw.Core31

--------------------------------------------------------------------------------

data ComparisonFunction =
     Never
   | Less
   | Equal
   | Lequal
   | Greater
   | Notequal
   | Gequal
   | Always
   deriving ( Eq, Ord, Show )

marshalComparisonFunction :: ComparisonFunction -> GLenum
marshalComparisonFunction x = case x of
   Never -> gl_NEVER
   Less -> gl_LESS
   Equal -> gl_EQUAL
   Lequal -> gl_LEQUAL
   Greater -> gl_GREATER
   Notequal -> gl_NOTEQUAL
   Gequal -> gl_GEQUAL
   Always -> gl_ALWAYS

unmarshalComparisonFunction :: GLenum -> ComparisonFunction
unmarshalComparisonFunction x
   | x == gl_NEVER = Never
   | x == gl_LESS = Less
   | x == gl_EQUAL = Equal
   | x == gl_LEQUAL = Lequal
   | x == gl_GREATER = Greater
   | x == gl_NOTEQUAL = Notequal
   | x == gl_GEQUAL = Gequal
   | x == gl_ALWAYS = Always
   | otherwise = error ("unmarshalComparisonFunction: illegal value " ++ show x)
