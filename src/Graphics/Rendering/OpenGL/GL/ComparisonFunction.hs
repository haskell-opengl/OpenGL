{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.ComparisonFunction
-- Copyright   :  (c) Sven Panne 2002-2019
-- License     :  BSD3
-- 
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
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

import Graphics.GL

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
   Never -> GL_NEVER
   Less -> GL_LESS
   Equal -> GL_EQUAL
   Lequal -> GL_LEQUAL
   Greater -> GL_GREATER
   Notequal -> GL_NOTEQUAL
   Gequal -> GL_GEQUAL
   Always -> GL_ALWAYS

unmarshalComparisonFunction :: GLenum -> ComparisonFunction
unmarshalComparisonFunction x
   | x == GL_NEVER = Never
   | x == GL_LESS = Less
   | x == GL_EQUAL = Equal
   | x == GL_LEQUAL = Lequal
   | x == GL_GREATER = Greater
   | x == GL_NOTEQUAL = Notequal
   | x == GL_GEQUAL = Gequal
   | x == GL_ALWAYS = Always
   | otherwise = error ("unmarshalComparisonFunction: illegal value " ++ show x)
