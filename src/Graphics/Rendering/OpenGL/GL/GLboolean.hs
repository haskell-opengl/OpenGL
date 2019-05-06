{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.GLboolean
-- Copyright   :  (c) Sven Panne 2002-2019
-- License     :  BSD3
-- 
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This is a purely internal module for (un-)marshaling GLboolean.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.GLboolean (
   marshalGLboolean, unmarshalGLboolean
) where

import Graphics.GL

--------------------------------------------------------------------------------

marshalGLboolean :: Num a => Bool -> a
marshalGLboolean x = fromIntegral $ case x of
   False -> GL_FALSE
   True -> GL_TRUE

unmarshalGLboolean :: (Eq a, Num a) => a -> Bool
unmarshalGLboolean = (/= fromIntegral GL_FALSE)
