{-# OPTIONS_HADDOCK hide #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.OpenGL.GL.Shaders.Shader
-- Copyright   :  (c) Sven Panne 2013
-- License     :  BSD3
-- 
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- This is a purely internal module for handling shader objects.
--
--------------------------------------------------------------------------------

module Graphics.Rendering.OpenGL.GL.Shaders.Shader (
   Shader(..)
) where

import Graphics.Rendering.OpenGL.GL.GLboolean
import Graphics.Rendering.OpenGL.GL.ObjectName
import Graphics.Rendering.OpenGL.Raw.Core31

--------------------------------------------------------------------------------

newtype Shader = Shader { shaderID :: GLuint }
   deriving ( Eq, Ord, Show )

instance ObjectName Shader where
   isObjectName = fmap unmarshalGLboolean . glIsShader . shaderID
   deleteObjectName = glDeleteShader . shaderID
