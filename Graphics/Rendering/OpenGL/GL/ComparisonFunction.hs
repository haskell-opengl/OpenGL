-- #hide
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.ComparisonFunction
-- Copyright   :  (c) Sven Panne 2002-2005
-- License     :  BSD-style (see the file libraries/OpenGL/LICENSE)
-- 
-- Maintainer  :  sven.panne@aedion.de
-- Stability   :  provisional
-- Portability :  portable
--
-- This is a purely internal module for (un-)marshaling ComparisonFunction.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.ComparisonFunction (
   ComparisonFunction(..), marshalComparisonFunction,
   unmarshalComparisonFunction
) where

import Graphics.Rendering.OpenGL.GL.BasicTypes ( GLenum )

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
   Never -> 0x200
   Less -> 0x201
   Equal -> 0x202
   Lequal -> 0x203
   Greater -> 0x204
   Notequal -> 0x205
   Gequal -> 0x206
   Always -> 0x207

unmarshalComparisonFunction :: GLenum -> ComparisonFunction
unmarshalComparisonFunction x
   | x == 0x200 = Never
   | x == 0x201 = Less
   | x == 0x202 = Equal
   | x == 0x203 = Lequal
   | x == 0x204 = Greater
   | x == 0x205 = Notequal
   | x == 0x206 = Gequal
   | x == 0x207 = Always
   | otherwise = error ("unmarshalComparisonFunction: illegal value " ++ show x)
